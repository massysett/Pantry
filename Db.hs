{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Keep functions in here as pure as possible. Use combinators to
-- mak
module Db where

import qualified Data.List as L
import qualified Data.Foldable as F
import Control.Applicative(Applicative, (<*>), pure)
import qualified Control.Monad.Error as E
import qualified Data.Traversable as T
import qualified Data.Text as X
import Control.Monad ((>=>))
import qualified Control.Monad as M
import qualified Data.DList as DL
import qualified Data.Map as Map
import qualified Control.Monad.State as St
import Data.Map ((!))
import Types(Next(next))
import Data.Serialize(Serialize, encode, decode)
import qualified Data.ByteString as BS
import System.IO(Handle)
import System.IO.Error(catchIOError)
import Control.Exception(IOException)
import qualified Data.Monoid as Monoid

import Food(Food, Error(MoveStartNotFound, MoveIdNotFound,
                        FileReadError, NotPantryFile,
                        WrongFileVersion, FileDecodeError,
                        FileSaveError),
            FoodId, foodId, oneFoodId)

newtype NextId = NextId { unNextId :: FoodId }
               deriving (Eq, Ord, Next, Serialize)
newtype Filename = Filename  { unFilename :: String }
                   deriving (Show, Serialize)
newtype Unsaved = Unsaved {unUnsaved :: Bool }

data Db = Db { dbNextId :: NextId
             , dbFilename :: Maybe Filename
             , dbUnsaved :: Unsaved
             , dbFoods :: DbFoods }

newtype Undos = Undos { unUndoList :: [Foods] }
newtype Foods = Foods { unFoods :: [Food] } deriving Serialize
instance Monoid.Monoid Foods where
  mappend (Foods l) (Foods r) = Foods $ l ++ r
  mempty = Foods []
newtype Volatile = Volatile { unVolatile :: Foods }
newtype DbFoods = DbFoods { unDbFoods :: Foods }
  
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
             , dbFoods = DbFoods . Foods $ [] }

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
predToFilter f (Volatile (Foods fs)) = Volatile . Foods . filter f $ fs

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

move :: FirstPos -> [FoodId] -> Foods -> Either Error Foods
move p is (Foods v) = do
  let pd fid food = fid == foodId food
  fs <- findManyWithFail MoveIdNotFound pd is v
  let sorted = sortByOrder is foodId fs
      deleted = deleteManyFirsts pd is v
  case p of
    Beginning -> return . Foods $ sorted ++ deleted
    (After aft) ->
      let (pre, suf) = L.break (pd aft) deleted
      in case suf of
        [] -> E.throwError $ MoveStartNotFound aft
        _ -> return . Foods $ pre ++ sorted ++ suf

------------------------------------------------------------
-- CHANGE TAGS AND PROPERTIES
------------------------------------------------------------

xformToFilterM :: (Food -> Either Error Food)
                 -> Volatile
                 -> Either Error Volatile
xformToFilterM f (Volatile (Foods fs)) =
  mapM f fs >>= return . Volatile . Foods

xformToTray :: (Volatile -> Either Error Volatile)
                 -> Tray -> Either Error Tray
xformToTray f t = do
  newV <- f . volatile $ t
  return t { volatile = newV }

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
  oldDbFoods = unDbFoods . dbFoods . trayDb $ t
  newUndos = Undos . take maxUndos . (oldDbFoods:)
             . unUndoList . undoList $ t

assignIds :: Foods -> NextId -> (Foods, NextId)
assignIds (Foods fs) n = (Foods r, n') where
  (r, n') = St.runState c n
  c = mapM assignId fs

append :: Tray -> Tray
append oldT = case (null . unFoods . volatile $ oldT) of
  True -> oldT
  False ->
    let (newWithId, newNextId) = assignIds (volatile oldT) oldNextId
        oldNextId = dbNextId . trayDb $ oldT
        oldDbFoods = dbFoods . trayDb $ oldT
        newFoods = oldDbFoods `Monoid.mappend` newWithId
        newT = installNewDbFoods newFoods newNextId oldT
    in newT
        
assignId :: Food -> St.State NextId Food
assignId f = do
  i <- St.get
  St.modify next
  return f { foodId = unNextId i }

replace :: Tray -> E.ErrorT Error IO Tray
replace t = return newT where
  newT = t { trayDb = oldDb { dbFoods = foods } }
  foods = volatile t
  oldDb = trayDb t

edit :: Tray -> E.ErrorT Error IO Tray
edit t = return newT where
  newT = t { trayDb = oldDb { dbFoods = Foods newFoods } }
  oldDbFoods = dbFoods oldDb
  oldDb = trayDb t
  newFoods = replaceManyFirsts eqId (unFoods . volatile $ t)
             (unFoods oldDbFoods)
  eqId f1 f2 = foodId f1 == foodId f2

delete :: Tray -> E.ErrorT Error IO Tray
delete t = return newT where
  newT = t { trayDb = oldDb { dbFoods = Foods newFoods } }
  oldDbFoods = dbFoods oldDb
  oldDb = trayDb t
  newFoods = deleteManyFirsts eqId (unFoods . volatile $ t)
             (unFoods oldDbFoods)
  eqId f1 f2 = foodId f1 == foodId f2

------------------------------------------------------------
-- OPEN AND SAVE FILES
------------------------------------------------------------
fileVersion :: BS.ByteString
fileVersion = BS.singleton 0

magic :: BS.ByteString
magic = BS.pack . map fromIntegral . map fromEnum $ "pantry"

-- | Writes a database to a handle. Any IO exceptions are caught and
-- returned as an Error; non-IO exceptions are not caught.
writeDb :: Handle -> Db -> E.ErrorT Error IO ()
writeDb h d = flip catchIOException FileSaveError $ do
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

------------------------------------------------------------
-- UTILITY BASEMENT
------------------------------------------------------------

sortByOrder :: (Ord a)
               => [a]
               -> (i -> a)
               -> [i]
               -> [i]
sortByOrder as f is = L.sortBy o is where
  m = Map.fromList $ zip as ([0..] :: [Int])
  o i1 i2 = compare (m ! (f i1)) (m ! (f i2))

findManyWithFail
  :: (E.Error e)
     => (p -> e)
     -- ^ Convert an item to find into an error, if it is not found

     -> (p -> a -> Bool)
     -- ^ How to make a predicate
     
     -> [p]
     -- ^ List of items for predicate
     
     -> [a]
     -- ^ List to search
     
     -> Either e [a]
findManyWithFail f p ps ls = M.sequence eithers where
  errors = L.map f ps
  preds = L.map p ps
  findfns = L.zipWith findWithFail errors preds
  eithers = findfns <*> pure ls

findWithFail :: e
                -> (a -> Bool)
                -> [a]
                -> Either e a
findWithFail e p ls = maybe (Left e) Right $ L.find p ls

deleteManyFirsts :: (p -> a -> Bool) -- ^ How to make a predicate
             -> [p]           -- ^ Source of what to delete
             -> [a]           -- ^ Target to delete in
             -> [a]           -- ^ Deleted
deleteManyFirsts pm rs = composed where
  preds = L.map pm rs
  delFns = L.map deleteFirst preds
  composed = compose delFns

deleteManyFirstsWithFail :: (E.Error e)
                            => (p -> e)
                            -> (p -> a -> Bool)
                            -> [p]
                            -> [a]
                            -> Either e [a]
deleteManyFirstsWithFail f pm rs = composed where
  preds = L.map pm rs
  delFns = L.zipWith (deleteFirstWithFail f) rs preds
  composed = composeM delFns

composeM :: (Monad m)
            => [a -> m a]
            -> a -> m a
composeM [] = return
composeM (f:fs) = f >=> (composeM fs)

deleteFirst :: (a -> Bool)  -- ^ Predicate
               -> [a]       -- ^ Delete from here
               -> [a]       -- ^ With first match deleted
deleteFirst p ls = case L.break p ls of
  (ns, []) -> ns
  (ns, as) -> ns ++ L.tail as

deleteFirstWithFail :: E.Error e
                       => (b -> e)
                       -> b
                       -> (a -> Bool)
                       -> [a]
                       -> (Either e [a])
deleteFirstWithFail f t p ls = case L.break p ls of
  (_, []) -> E.throwError $ f t
  (ns, as) -> return $ ns ++ L.tail as

replaceManyFirsts :: (a -> a -> Bool) -- ^ How to make a predicate
              -> [a]             -- ^ Source of replacements
              -> [a]             -- ^ Target to replace within
              -> [a]
replaceManyFirsts pm rs = composed where
  preds = L.map pm rs
  replFns = L.zipWith replaceFirst preds rs
  composed = compose replFns

replaceFirst :: (a -> Bool) -- ^ Predicate
                -> a        -- ^ Replacement
                -> [a]      -- ^ Replace within this list
                -> [a]
replaceFirst p r ls = case L.break p ls of
  (ns, []) -> ns
  (ns, as) -> ns ++ r : L.tail as

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
