{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Keep functions in here as pure as possible. Use combinators to
-- mak
module Db where

import qualified Data.List as L
import qualified Data.Foldable as F
import Control.Applicative(Applicative, (<*>), (<**>), pure, (<$>))
import qualified Control.Monad.Error as E
import qualified Data.Text as X
import Control.Monad ((>=>))
import qualified Data.DList as DL
import qualified Data.Map as Map
import qualified Control.Monad.State as St
import Data.Map ((!))
import Types(Next(next))
import Data.Serialize(Serialize, encode, decode)
import qualified Data.ByteString as BS
import System.IO(hSetBinaryMode, withFile, IOMode(WriteMode))
import System.IO.Error(catchIOError)
import Control.Exception(IOException)
import Data.Maybe(catMaybes)
import qualified Data.Set as Set

import Food(Food, Error(MoveStartNotFound, MoveIdNotFound,
                        FileReadError, NotPantryFile,
                        WrongFileVersion, FileDecodeError,
                        FileSaveError, NoSaveFilename,
                        MultipleMoveIdMatches, MultipleEditIdMatches),
            FoodId, foodId, oneFoodId, unIngr, ingr, Ingr(Ingr))
import Data.Monoid(mconcat)

newtype NextId = NextId { unNextId :: FoodId }
               deriving (Eq, Ord, Next, Serialize)
newtype Filename = Filename  { unFilename :: String }
                   deriving (Show, Serialize)
newtype Unsaved = Unsaved {unUnsaved :: Bool }

data Db = Db { dbNextId :: NextId
             , dbFilename :: Maybe Filename
             , dbUnsaved :: Unsaved
             , dbFoods :: DbFoods }

newtype Undos = Undos { unUndoList :: [DbFoods] }
newtype Volatile = Volatile { unVolatile :: [Food] }
newtype DbFoods = DbFoods { unDbFoods :: [Food] } deriving Serialize
  
data Tray = Tray { trayDb :: Db
                 , volatile :: Volatile
                 , undoList :: Undos
                 , done :: Done
                 , output :: DL.DList X.Text }

type Convey = Tray -> E.ErrorT Error IO Tray

blankDb :: Db
blankDb = Db { dbNextId = NextId oneFoodId
             , dbFilename = Nothing
             , dbUnsaved = Unsaved False
             , dbFoods = DbFoods [] }

loadTray :: Db -> Undos -> Tray
loadTray d u = Tray { trayDb = d
                    , volatile = Volatile . unDbFoods . dbFoods $ d
                    , undoList = u
                    , done = NotDone
                    , output = DL.empty }

data Done = Done | NotDone

------------------------------------------------------------
-- FILTERING
------------------------------------------------------------

predToFilter :: (Food -> Bool) -> Volatile -> Volatile
predToFilter f (Volatile fs) = Volatile . filter f $ fs

filterToTrayFilter :: (Volatile -> Volatile) -> Tray -> Tray
filterToTrayFilter f t = t { volatile = f . volatile $ t }

trayFilterToConvey :: (Tray -> Tray) -> Tray -> E.ErrorT Error IO Tray
trayFilterToConvey = impurify

predToConvey :: (Food -> Bool) -> Tray -> E.ErrorT Error IO Tray
predToConvey = trayFilterToConvey . filterToTrayFilter . predToFilter

filterToConvey :: (Volatile -> Volatile) -> Tray -> E.ErrorT Error IO Tray
filterToConvey = trayFilterToConvey . filterToTrayFilter

newVolatileToConvey :: Volatile -> Tray -> E.ErrorT Error IO Tray
newVolatileToConvey = trayFilterToConvey . filterToTrayFilter . const

data FirstPos = Beginning | After FoodId

concatMoveIds ::
  [[Food]] -- ^ Output from findAllInOrder
  -> [FoodId] -- ^ FoodId items to look up
  -> Either Error [Food] -- ^ Concatenated result, or error
concatMoveIds fss is = F.foldrM f [] (zip is fss) where
  f (i, []) _ = E.throwError $ MoveIdNotFound i
  f (_, (food:[])) rs = return $ food : rs
  f (i, _) _ = E.throwError $ MultipleMoveIdMatches i

move :: FirstPos -> [FoodId] -> Volatile -> Either Error Volatile
move p is (Volatile v) = do
  let pd fid food = fid == foodId food
      finds = findAllInOrder pd is v
  fs <- concatMoveIds finds is
  let sorted = sortByOrder is foodId fs
      deleted = deleteAll foodId is v
  case p of
    Beginning -> return . Volatile $ sorted ++ deleted
    (After aft) ->
      let (pre, suf) = L.break (pd aft) deleted
      in case suf of
        [] -> E.throwError $ MoveStartNotFound aft
        _ -> return . Volatile $ pre ++ sorted ++ suf

------------------------------------------------------------
-- CHANGE TAGS AND PROPERTIES, NUTRIENTS, AVAIL UNITS
------------------------------------------------------------

xformToFilterM :: (Food -> Either Error Food)
                 -> Volatile
                 -> Either Error Volatile
xformToFilterM f (Volatile fs) =
  mapM f fs >>= return . Volatile

filterMToTrayM :: (Volatile -> Either Error Volatile)
                 -> Tray -> Either Error Tray
filterMToTrayM f t = do
  newV <- f . volatile $ t
  return t { volatile = newV }

trayMToConvey :: (Tray -> Either Error Tray)
                 -> Tray -> E.ErrorT Error IO Tray
trayMToConvey f t = liftToErrorT . f $ t

xformToConvey :: (Food -> Either Error Food)
                 -> Tray -> E.ErrorT Error IO Tray
xformToConvey = trayMToConvey . filterMToTrayM . xformToFilterM

------------------------------------------------------------
-- INGREDIENTS
------------------------------------------------------------
replaceWithIngr :: Volatile -> Volatile
replaceWithIngr (Volatile fs) = Volatile n where
  n = unIngr . mconcat . map ingr $ fs

removeIngr :: Volatile -> Volatile
removeIngr (Volatile fs) = Volatile ns where
  ns = map g fs
  g f = f { ingr = Ingr [] }

------------------------------------------------------------
-- ADDING CHANGED FOODS
------------------------------------------------------------

maxUndos :: Int
maxUndos = 15

-- | Installs new changed Foods. Assumes there are actually changes to
-- install--that is, it always marks the Db as Unsaved and always
-- changes the undo list. Functions that call this function should not
-- call it if there actually are no changes to install.
installNewDbFoods :: DbFoods -- ^ New db foods
                     -> NextId -- ^ New NextId
                     -> Tray  -- ^ Old tray
                     -> Tray  -- ^ New tray
installNewDbFoods f n t = newT where
  newT = t { trayDb = newDb
           , undoList = newUndos }
  newDb = (trayDb t) { dbNextId = n
                     , dbUnsaved = Unsaved True
                     , dbFoods = f }
  oldDbFoods = dbFoods . trayDb $ t
  newUndos = Undos . take maxUndos . (oldDbFoods:)
             . unUndoList . undoList $ t

assignIds :: [Food] -> NextId -> ([Food], NextId)
assignIds fs n = St.runState c n where
  c = mapM assignId fs

assignId :: Food -> St.State NextId Food
assignId f = do
  i <- St.get
  St.modify next
  return f { foodId = unNextId i }

-- | Takes the Volatile from a Tray and assigns new IDs to it and
-- combines the Volatile with the DB foods in the tray, using the
-- given combining function. Returns a new tray with Volatile
-- unchanged, a new Db, and a new NextID. Does nothing if Volatile is
-- null.
--
-- The combining function might prepend new foods, append new foods,
-- or junk the existing foods altogether.
volatileToDb :: (DbFoods -> [Food] -> DbFoods) -> Tray -> Tray
volatileToDb combine oldT = case (null . unVolatile . volatile $ oldT) of
  True -> oldT
  False ->
    let (newWithId, newNextId) = assignIds oldV oldNextId
        (Volatile oldV) = volatile oldT
        oldNextId = dbNextId . trayDb $ oldT
        oldDbFoods = dbFoods . trayDb $ oldT
        newFoods = combine oldDbFoods newWithId
        newT = installNewDbFoods newFoods newNextId oldT
    in newT

append :: Tray -> Tray
append = volatileToDb f where
  f (DbFoods ds) fs = DbFoods $ ds ++ fs

prepend :: Tray -> Tray
prepend = volatileToDb f where
  f (DbFoods ds) fs = DbFoods $ fs ++ ds

replace :: Tray -> Tray
replace = volatileToDb f where
  f _ fs = DbFoods fs
        
edit :: Tray -> Either Error Tray
edit t =
  case (null. unVolatile . volatile $ t) of
    (True) -> return t
    (False) -> newT
  where
    newT = f (volatile t) oldDbFoods >>= \newFoods ->
           return t { trayDb = oldDb { dbFoods = newFoods
                                     , dbUnsaved = Unsaved True }
                    , undoList = newUndo }
    newUndo = Undos l where
      l = take maxUndos $ oldDbFoods : (unUndoList . undoList $ t)
    oldDbFoods = dbFoods oldDb
    oldDb = trayDb t
    f (Volatile v) (DbFoods d) =
      case replaceAll foodId v d of
        (Left k) -> E.throwError $ MultipleEditIdMatches k
        (Right vs) -> return . DbFoods $ vs

delete :: Tray -> Tray
delete t =
  case (null. unVolatile . volatile $ t) of
    (True) -> t
    (False) -> newT
  where
    newT = t { trayDb = oldDb { dbFoods = newFoods
                              , dbUnsaved = Unsaved True }
             , undoList = newUndo }
    newUndo = Undos l where
      l = take maxUndos $ oldDbFoods : (unUndoList . undoList $ t)
    oldDbFoods = dbFoods oldDb
    oldDb = trayDb t
    newFoods = f (volatile t) oldDbFoods
    f (Volatile v) (DbFoods d) =
      DbFoods $ deleteAll foodId (map foodId v) d

-- | True if two foods have equal IDs.
eqId :: Food -> Food -> Bool
eqId f1 f2 = foodId f1 == foodId f2

------------------------------------------------------------
-- OPEN AND SAVE FILES
------------------------------------------------------------
fileVersion :: BS.ByteString
fileVersion = BS.singleton 0

magic :: BS.ByteString
magic = BS.pack . map fromIntegral . map fromEnum $ "pantry"

-- | Writes a database to a file. Any IO exceptions are caught and
-- returned as an Error; non-IO exceptions are not caught.
writeDb :: Filename -> Db -> E.ErrorT Error IO ()
writeDb (Filename f) d = flip catchIOException FileSaveError c where
  c = withFile f WriteMode $ \h -> do
    hSetBinaryMode h True
    BS.hPut h magic
    BS.hPut h fileVersion
    BS.hPut h $ encode (dbNextId d, dbFoods d)

-- | Reads a file from disk. Catches any IO errors and puts them on an
-- Error; these are returned as Left Error. Successful reads are
-- returned as Right ByteString. Any non-IO errors are not caught.
readBS :: Filename -> E.ErrorT Error IO BS.ByteString
readBS (Filename f) = catchIOException (BS.readFile f) FileReadError

-- | Carries out an IO action. Takes any IOExceptions, catches them,
-- and puts them into an IO Either. Non IOException exceptions are not
-- caught.
catchIOException :: IO a
                    -> (IOException -> Error)
                    -> E.ErrorT Error IO a
catchIOException a f = E.ErrorT $ catchIOError
                       (a >>= return . Right) (return . Left . f)

-- | Decode a ByteString to a Db. Not in IO monad.
decodeBSWithHeader :: Filename -> BS.ByteString -> Either Error Db
decodeBSWithHeader f bs = do
  E.unless (magic `BS.isPrefixOf` bs) (E.throwError NotPantryFile)
  let noMagic = BS.drop (BS.length magic) bs
  E.unless (fileVersion `BS.isPrefixOf` noMagic)
    (E.throwError WrongFileVersion)
  let noHeader = BS.drop (BS.length fileVersion) noMagic
  case decode noHeader of
    (Right (i, fs)) -> return $ Db { dbNextId = i
                                   , dbFilename = Just f
                                   , dbUnsaved = Unsaved False
                                   , dbFoods = fs }
    (Left s) -> E.throwError $ FileDecodeError s

-- | Reads a database. Any IO errors are caught and returned in an
-- appropriate Error. Non-IO exceptions are not caught (there should
-- not be any...but if there are they are not caught.)  Do not
-- canonicalize the input filename. This must happen on the client
-- side.
readDb :: Filename -> E.ErrorT Error IO Db
readDb f = do
  bs <- readBS f
  db <- liftToErrorT $ decodeBSWithHeader f bs
  return db

open :: Filename -> Tray -> E.ErrorT Error IO Tray
open f t = do
  d <- readDb f
  return t { trayDb = d
           , undoList = addToUndos (dbFoods . trayDb $ t) (undoList t)
           }
    
saveWithFilename :: Filename -> Tray -> E.ErrorT Error IO Tray
saveWithFilename f t = do
    writeDb f . trayDb $ t
    let newDb = (trayDb t) { dbUnsaved = Unsaved False
                           , dbFilename = Just f }
        newTray = t { trayDb = newDb }
    return newTray

save :: Tray -> E.ErrorT Error IO Tray
save t = case dbFilename . trayDb $ t of
  (Nothing) -> E.throwError NoSaveFilename
  (Just f) -> saveWithFilename f t

saveAs :: Filename -> Tray -> E.ErrorT Error IO Tray
saveAs = saveWithFilename

addToUndos :: DbFoods -> Undos -> Undos
addToUndos d = Undos . take maxUndos . (d :) . unUndoList

-- | Given a function that combines the old DbFoods with the new
-- DbFoods, carry out a prepend or append operation. All prepended or
-- appended foods must be assigned new IDs (appendOrPrepend takes care
-- of the renumbering).
appendOrPrependPure :: (DbFoods -> [Food] -> DbFoods) -- ^ Combiner
                   -> [Food]   -- ^ Loaded foods
                   -> Tray -- ^ Old tray
                   -> Tray
appendOrPrependPure f fLoaded t = tN where
  tN = t { trayDb = newDb
         , undoList = newUndo }
  newDb = d { dbNextId = newNextId
            , dbUnsaved = Unsaved True
            , dbFoods = fN }
  d = trayDb t
  (fNumbered, newNextId) = assignIds fLoaded (dbNextId d)
  newUndo = addToUndos (dbFoods d) (undoList t)
  fN = f (dbFoods d) fNumbered

appendOrPrepend :: (DbFoods -> [Food] -> DbFoods)
                   -> Filename
                   -> Tray
                   -> E.ErrorT Error IO Tray
appendOrPrepend g f t = do
  d <- readDb f
  return $ appendOrPrependPure g (unDbFoods . dbFoods $ d) t

appendFile :: Filename -> Tray -> E.ErrorT Error IO Tray
appendFile = appendOrPrepend (\(DbFoods l) r -> DbFoods $ l ++ r)

prependFile :: Filename -> Tray -> E.ErrorT Error IO Tray
prependFile = appendOrPrepend (\(DbFoods l) r -> DbFoods $ r ++ l)

close :: Tray -> Tray
close t = tN where
  tN = t { trayDb = newDb
         , undoList = newUndo }
  newDb = blankDb
  d = trayDb t
  newUndo = addToUndos (dbFoods d) (undoList t)

quit :: Tray -> Tray
quit t = t { done = Done }

------------------------------------------------------------
-- UTILITY BASEMENT
------------------------------------------------------------

-- | Delete all the items from a list that match one of several
-- predicates.
deleteAll ::
  (Ord k)
  => (v -> k) -- ^ How to get a key from a value
  -> [k]      -- ^ Delete items matching these keys
  -> [v]      -- ^ delete from here
  -> [v]
deleteAll f ks vs = catMaybes maybes where
  mp = Set.fromList ks
  maybes = map toMaybe vs
  toMaybe i = case Set.member (f i) mp of
    True -> Just i
    False -> Nothing

-- | For each item in a list, replace an item in a different list.
replaceAll ::
  (Ord k)
  => (v -> k)
  -- ^ How to get a key from a value
  
  -> [v]
  -- ^ source of replacements
  
  -> [v]
  -- ^ replace items within this list

  -> Either k [v]
  -- ^ The computation fails if two of the items in the source of
  -- replacements yield the same key. Otherwise, returns a list of
  -- items with the replacements made.
replaceAll f ss ts =
  let ps = map (\v -> (f v, v)) ss
  in case fromListNoDupe ps of
    (Left k) -> Left k
    (Right mp) ->
      let g v a = Map.findWithDefault v (f v) mp : a
      in Right $ foldr g [] ts

-- | Build a map from a list of keys and values, but returns Left k
-- for the first duplicate key if a duplicate key is found.
fromListNoDupe :: (Ord k) => [(k, v)] -> Either k (Map.Map k v)
fromListNoDupe = foldr f (Right Map.empty) where
  f _ (Left e) = Left e
  f (k, v) (Right m) =
    let fv _ n _ = n
    in case Map.insertLookupWithKey fv k v m of
      (Just _, _) -> Left k
      (Nothing, nm) -> Right nm

-- | Find all items from a given list inside of another list, using a
-- specified predicate. 
findAllInOrder ::
  (k -> a -> Bool)
  -- ^ How to make a predicate
  
  -> [k]
  -- ^ Find items matching these keys

  -> [a]
  -- ^ Find items within this list
  
  -> [[a]]
  -- ^ Items matching each key, in order
findAllInOrder f ks as = f <$> ks <**> pure filter <*> pure as

sortByOrder :: (Ord a)
               => [a]
               -> (i -> a)
               -> [i]
               -> [i]
sortByOrder as f is = L.sortBy o is where
  m = Map.fromList $ zip as ([0..] :: [Int])
  o i1 i2 = compare (m ! (f i1)) (m ! (f i2))

composeM :: (Monad m)
            => [a -> m a]
            -> a -> m a
composeM [] = return
composeM (f:fs) = f >=> (composeM fs)

compose :: [a -> a] -> (a -> a)
compose = F.foldl (flip (.)) id

liftToErrorT :: (E.Error e, Monad m) => Either e a -> E.ErrorT e m a
liftToErrorT e = case e of
  (Left err) -> E.throwError err
  (Right good) -> return good

impurify :: Monad m => (a -> b) -> a -> m b
impurify f a = return $ f a

-- Local Variables:
-- compile-command: "ghc -Wall -outputdir temp Db.hs"
-- End:
