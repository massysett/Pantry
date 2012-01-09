{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Keep functions in here as pure as possible. Then, use combinators to
-- make pure functions into Convey functions.
module Pantry.Conveyor where

import qualified Data.List as L
import Data.Foldable ( Foldable, foldrM )
import Control.Applicative(Applicative, (<*>), (<**>), pure, (<$>))
import qualified Control.Monad.Error as E
import Control.Monad ((>=>))
import qualified Data.DList as DL
import qualified Data.Map as Map
import qualified Control.Monad.State as St
import Data.Map ((!))
import Pantry.Types(Next(next), NonNegInteger(unNonNegInteger),
                    posMixedOne)
import Data.Serialize(encode, decode)
import qualified Data.ByteString as BS
import System.IO(hSetBinaryMode, withFile, IOMode(WriteMode))
import System.IO.Error (catch)
import Control.Exception(IOException, evaluate)
import Data.Maybe(catMaybes)
import qualified Data.Set as Set
import qualified Data.Text as X
import qualified Pantry.Sorter as S
import Pantry.Paths ( CanonPath, unCanonPath )
import qualified Pantry.Error as R

import qualified Pantry.Food as F
import Pantry.Food(Food, unIngr, Ingr, FoodId)
import Data.Monoid(mconcat)
import Pantry.Bag ( NextId(unNextId),
             Unsaved(Unsaved),
             Buffer(Buffer, unBuffer),
             Undos(Undos, unUndos))
import Pantry.Tray ( Tray, nextId, filename, unsaved,
                     buffer, undos, volatile, done, output,
                     unOutput, blankTray, clientCurrDir,
                     Volatile(Volatile), unVolatile,
                     Output(Output),
                     Done(Done),
                     compact)
import Pantry.Reports (ReportGroups, printReportGroups)
import Pantry.Reports.Types ( ReportOpts )
import qualified Pantry.Reports as R

-- * The conveyor
type Convey = Tray -> E.ErrorT R.Error IO Tray

------------------------------------------------------------
-- * Combinators
------------------------------------------------------------

predToFilter :: (Food -> Bool) -> Volatile -> Volatile
predToFilter f (Volatile fs) = Volatile . filter f $ fs

filterToTrayFilter :: (Volatile -> Volatile) -> Tray -> Tray
filterToTrayFilter f t = t { volatile = f . volatile $ t }

printerToTrayFilter :: (Tray -> DL.DList X.Text) -> Tray -> Tray
printerToTrayFilter f t = t { output = o } where
  o = Output . DL.append (unOutput . output $ t) . f $ t

trayFilterToConvey :: (Tray -> Tray) -> Tray -> E.ErrorT R.Error IO Tray
trayFilterToConvey = impurify

predToConvey :: (Food -> Bool) -> Tray -> E.ErrorT R.Error IO Tray
predToConvey = trayFilterToConvey . filterToTrayFilter . predToFilter

filterToConvey :: (Volatile -> Volatile) -> Tray -> E.ErrorT R.Error IO Tray
filterToConvey = trayFilterToConvey . filterToTrayFilter

newVolatileToConvey :: Volatile -> Tray -> E.ErrorT R.Error IO Tray
newVolatileToConvey = trayFilterToConvey . filterToTrayFilter . const

------------------------------------------------------------
-- * Filtering foods from volatile
------------------------------------------------------------
-- | Remove all foods from volatile that match one of the IDs
-- given. Fails if one of the IDs is not found.
--
-- Works by running a right fold over the FoodId list. The accumulator
-- in the fold looks up the FoodId in a map which maps all the FoodIds
-- in Volatile to the foods in Volatile. If the food is found, it is
-- added to the result; if not, the FoodId is added to a list of
-- FoodIds not found. Therefore the foods returned are in the order of
-- the FoodId given. Also, it is not an error if a FoodId is given
-- more than once--the food is simply in the list more than once.
findIds :: [FoodId] -> Volatile -> Either R.Error Volatile
findIds is (Volatile v) = let
  g m i (Left es) = case Map.lookup i m of
    (Just _) -> Left es
    (Nothing) -> Left (i : es)
  g m i (Right fs) = case Map.lookup i m of
    (Just found) -> Right (found : fs)
    Nothing -> Left [i]
  ei = foldr (g foodMap) (Right []) is
  foodMap = Map.fromList $ zip (map F.getFoodId v) v
  in case ei of
    (Left es) -> Left $ R.IDsNotFound es
    (Right fs) -> Right $ Volatile fs

clear :: Volatile
clear = Volatile []

data FirstPos = Beginning | After FoodId

concatMoveIds ::
  [[Food]] -- ^ Output from findAllInOrder
  -> [FoodId] -- ^ FoodId items to look up
  -> Either R.Error [Food] -- ^ Concatenated result, or error
concatMoveIds fss is = foldrM f [] (zip is fss) where
  f (i, []) _ = E.throwError $ R.MoveIdNotFound i
  f (_, (food:[])) rs = return $ food : rs
  f (i, _) _ = E.throwError $ R.MultipleMoveIdMatches i

recopy :: Tray -> Tray
recopy t = t { volatile = Volatile . unBuffer . buffer $ t }

head :: NonNegInteger -> Volatile -> Volatile
head i (Volatile v) = Volatile $ L.genericTake (unNonNegInteger i) v

tail :: NonNegInteger -> Volatile -> Volatile
tail i (Volatile v) = Volatile $ L.genericDrop
                      (L.genericLength v - unNonNegInteger i) v

create :: Volatile -> Volatile
create (Volatile vs) = Volatile $ vs ++ [e] where
  e = F.emptyFood $ F.CurrUnit n a
  n = F.UnitName . X.pack $ "grams"
  a = F.UnitAmt . F.PosMixedGrams $ posMixedOne

move :: FirstPos -> [FoodId] -> Volatile -> Either R.Error Volatile
move p is (Volatile v) = do
  let pd fid food = fid == F.getFoodId food
      finds = findAllInOrder pd is v
  fs <- concatMoveIds finds is
  let sorted = sortByOrder is F.getFoodId fs
      deleted = deleteAll F.getFoodId is v
  case p of
    Beginning -> return . Volatile $ sorted ++ deleted
    (After aft) ->
      let (pre, suf) = L.break (pd aft) deleted
      in case suf of
        [] -> E.throwError $ R.MoveStartNotFound aft
        _ -> return . Volatile $ pre ++ sorted ++ suf

undo :: NonNegInteger -> Tray -> Either R.Error Tray
undo n t =
  case unNonNegInteger n < L.genericLength (unUndos . undos $ t) of
    False ->
      Left $ R.UndoTooBig n (L.genericLength (unUndos . undos $ t))
    True ->
      let vn = Volatile . unBuffer $ b
          b = L.genericIndex (unUndos . undos $ t) (unNonNegInteger n)
      in Right t { volatile = vn }

------------------------------------------------------------
-- CHANGE TAGS AND PROPERTIES, NUTRIENTS, AVAIL UNITS
------------------------------------------------------------

xformToFilterM :: (Food -> Either R.Error Food)
                 -> Volatile
                 -> Either R.Error Volatile
xformToFilterM f (Volatile fs) =
  mapM f fs >>= return . Volatile

filterMToTrayM :: (Volatile -> Either R.Error Volatile)
                 -> Tray -> Either R.Error Tray
filterMToTrayM f t = do
  newV <- f . volatile $ t
  return t { volatile = newV }

trayMToConvey :: (Tray -> Either R.Error Tray)
                 -> Tray -> E.ErrorT R.Error IO Tray
trayMToConvey f t = liftToErrorT . f $ t

xformToConvey :: (Food -> Either R.Error Food)
                 -> Tray -> E.ErrorT R.Error IO Tray
xformToConvey = trayMToConvey . filterMToTrayM . xformToFilterM

------------------------------------------------------------
-- INGREDIENTS
------------------------------------------------------------
replaceWithIngr :: Volatile -> Volatile
replaceWithIngr (Volatile fs) = Volatile n where
  n = unIngr . mconcat . map F.getIngr $ fs

removeIngr :: Volatile -> Volatile
removeIngr (Volatile fs) = Volatile ns where
  ns = map g fs
  g f = F.setIngr (F.Ingr []) f

-- | Finds foods in the the buffer that have the FoodId given. Adds
-- each food found to the ingredients of all the foods in
-- Volatile. Returns an error if one of the FoodId given does not
-- match a food in the buffer.
ingrToVolatile :: [FoodId] -> Tray -> Either R.Error Tray
ingrToVolatile is t = case lookupFoodId is t of
  (Left es) -> Left (R.IngrToVolatileLookup es)
  (Right fs) -> Right $ addIngrToVolatile fs t

-- | Takes a list of foods and appends them as ingredients to each
-- food in volatile.
addIngrToVolatile :: [Food] -> Tray -> Tray
addIngrToVolatile fs t = t { volatile = v } where
  v = Volatile $ map addFoods (unVolatile . volatile $ t)
  addFoods f = F.setIngr newIngr f where
    newIngr = F.Ingr ((F.unIngr . F.getIngr $ f) ++ fs)

-- | Looks up in the buffer the foods corresponding to a list of
-- FoodId given. Returns Right [Food] if all the foods are found;
-- returns Left R.Error if any of the FoodId did not match a food.
lookupFoodId :: [FoodId] -> Tray -> Either [FoodId] [Food]
lookupFoodId is t = let
  b = unBuffer . buffer $ t
  m = Map.fromList $ zip (map F.getFoodId b) b
  f i (Left es) = case Map.lookup i m of
    Nothing -> Left (i:es)
    (Just _) -> Left es
  f i (Right fs) = case Map.lookup i m of
    Nothing -> Left [i]
    (Just fd) -> Right (fd:fs)
  in foldr f (Right []) is

------------------------------------------------------------
-- REPORTING
------------------------------------------------------------
report :: ReportOpts -> ReportGroups -> Tray -> Tray
report o g t = t { output = Output (DL.append old new) } where
  old = unOutput . output $ t
  new = printReportGroups o g t

------------------------------------------------------------
-- SORTING
------------------------------------------------------------
sort :: Foldable f => S.TagMap -> f S.Key -> Volatile -> Volatile
sort ts ks (Volatile v) = Volatile $ L.sortBy f v where
  f = S.foodcmp ts ks

------------------------------------------------------------
-- ADDING CHANGED FOODS
------------------------------------------------------------

maxUndos :: Int
maxUndos = 15

-- | Installs new changed Foods. Assumes there are actually changes to
-- install--that is, it always marks the Db as Unsaved and always
-- changes the undo list. Functions that call this function should not
-- call it if there actually are no changes to install.
installNewDbFoods :: Buffer -- ^ New db foods
                     -> NextId -- ^ New NextId
                     -> Tray  -- ^ Old tray
                     -> Tray  -- ^ New tray
installNewDbFoods f n t = newT where
  newT = t { nextId = n
           , unsaved = Unsaved True
           , buffer = f
           , undos = newUndos }
  newUndos = Undos . take maxUndos . (buffer t :)
             . unUndos . undos $ t

assignIds :: [Food] -> NextId -> ([Food], NextId)
assignIds fs n = St.runState c n where
  c = mapM assignId fs

assignId :: Food -> St.State NextId Food
assignId f = do
  i <- St.get
  St.modify next
  return $ F.setFoodId (unNextId i) f

-- | Takes the Volatile from a Tray and assigns new IDs to it and
-- combines the Volatile with the DB foods in the tray, using the
-- given combining function. Returns a new tray with Volatile
-- unchanged, a new Db, and a new NextID. Does nothing if Volatile is
-- null.
--
-- The combining function might prepend new foods, append new foods,
-- or junk the existing foods altogether.
volatileToDb :: (Buffer -> [Food] -> Buffer) -> Tray -> Tray
volatileToDb combine oldT = case (null . unVolatile . volatile $ oldT) of
  True -> oldT
  False ->
    let (newWithId, newNextId) = assignIds oldV oldNextId
        (Volatile oldV) = volatile oldT
        oldNextId = nextId oldT
        oldDbFoods = buffer oldT
        newFoods = combine oldDbFoods newWithId
        newT = installNewDbFoods newFoods newNextId oldT
    in newT

append :: Tray -> Tray
append = volatileToDb f where
  f (Buffer ds) fs = Buffer $ ds ++ fs

prepend :: Tray -> Tray
prepend = volatileToDb f where
  f (Buffer ds) fs = Buffer $ fs ++ ds

replace :: Tray -> Tray
replace = volatileToDb f where
  f _ fs = Buffer fs
        
edit :: Tray -> Either R.Error Tray
edit t =
  case (null. unVolatile . volatile $ t) of
    (True) -> return t
    (False) -> newT
  where
    newT = f (volatile t) (buffer t) >>= \newFoods ->
      return t { buffer = newFoods
               , unsaved = Unsaved True
               , undos = newUndo }
    newUndo = Undos l where
      l = take maxUndos $ buffer t : (unUndos . undos $ t)
    f (Volatile v) (Buffer d) =
      case replaceAll F.getFoodId v d of
        (Left k) -> E.throwError $ R.MultipleEditIdMatches k
        (Right vs) -> return . Buffer $ vs

delete :: Tray -> Tray
delete t =
  case (null. unVolatile . volatile $ t) of
    (True) -> t
    (False) -> newT
  where
    newT = t { buffer = newFoods
             , unsaved = Unsaved True
             , undos = newUndo }
    newUndo = Undos l where
      l = take maxUndos $ buffer t : (unUndos . undos $ t)
    newFoods = f (volatile t) (buffer t)
    f (Volatile v) (Buffer d) =
      Buffer $ deleteAll F.getFoodId (map F.getFoodId v) d

-- | Adds an ingredient to a food.
addIngredient :: Food -- ^ Ingredient to add
                 -> Food -- ^ Add it to this food
                 -> Food
addIngredient i f = F.setIngr is f where
  is = F.Ingr . (++ [i]) . F.unIngr . F.getIngr $ f

-- | Add foods in volatile to foods in the buffer that match a given
-- ID. Fails if one of the given FoodIds is not found.
ingrFromVolatile :: [FoodId] -> Tray -> Either R.Error Tray
ingrFromVolatile is t = let
  add f = foldl (flip addIngredient) f (unVolatile . volatile $ t)
  --add = addIngredients (unVolatile . volatile $ t)
  g f = do
    s <- St.get
    case Set.member (F.getFoodId f) s of
      False -> return f
      True -> do
        St.modify (Set.delete (F.getFoodId f))
        return $ add f
  bufComp = mapM g (unBuffer . buffer $ t)
  initSet = Set.fromList is
  (newBufFoods, newSet) = St.runState bufComp initSet
  newTray = t { buffer = Buffer newBufFoods }
  err = R.IngrFromVolatileNotFound (Set.toList newSet)
  in case (Set.null newSet) of
    True -> Right newTray
    False -> Left err


-- | True if two foods have equal IDs.
eqId :: Food -> Food -> Bool
eqId f1 f2 = F.getFoodId f1 == F.getFoodId f2

------------------------------------------------------------
-- OPEN AND SAVE FILES
------------------------------------------------------------
fileVersion :: BS.ByteString
fileVersion = BS.singleton 0

magic :: BS.ByteString
magic = BS.pack . map fromIntegral . map fromEnum $ "pantry"

-- | Writes a database to a file. Any IO exceptions are caught and
-- returned as an Error; non-IO exceptions are not caught.
writeDb :: CanonPath -> NextId -> Buffer -> E.ErrorT R.Error IO ()
writeDb f n b = flip catchIOException R.FileSaveError c where
  c = withFile (unCanonPath f) WriteMode $ \h -> do
    hSetBinaryMode h True
    BS.hPut h magic
    BS.hPut h fileVersion
    BS.hPut h $ encode (n, b)

-- | Reads a file from disk. Catches any IO errors and puts them on an
-- Error; these are returned as Left Error. Successful reads are
-- returned as Right ByteString. Any non-IO errors are not caught.
readBS :: CanonPath -> E.ErrorT R.Error IO BS.ByteString
readBS f = catchIOException (BS.readFile . unCanonPath $ f)
           R.FileReadError

-- | Carries out an IO action. Takes any IOExceptions, catches them,
-- and puts them into an IO Either. Non IOException exceptions are not
-- caught.
catchIOException :: IO a
                    -> (IOException -> R.Error)
                    -> E.ErrorT R.Error IO a
catchIOException a f = E.ErrorT $ System.IO.Error.catch
                       (a >>= return . Right) (return . Left . f)

-- | Decode a ByteString to a Db. Not in IO monad.
decodeBSWithHeader :: BS.ByteString -> Either R.Error (NextId, Buffer)
decodeBSWithHeader bs = do
  E.unless (magic `BS.isPrefixOf` bs) (E.throwError R.NotPantryFile)
  let noMagic = BS.drop (BS.length magic) bs
  E.unless (fileVersion `BS.isPrefixOf` noMagic)
    (E.throwError R.WrongFileVersion)
  let noHeader = BS.drop (BS.length fileVersion) noMagic
  case decode noHeader of
    (Right (i, fs)) -> return (i, fs)
    (Left s) -> E.throwError $ R.FileDecodeError (X.pack s)

-- | Reads a database. Any IO errors are caught and returned in an
-- appropriate Error. Non-IO exceptions are not caught (there should
-- not be any...but if there are they are not caught.)  Do not
-- canonicalize the input filename. This must happen on the client
-- side.
readDb :: CanonPath -> E.ErrorT R.Error IO (NextId, Buffer)
readDb f = readBS f >>= (liftToErrorT . decodeBSWithHeader)

open :: CanonPath -> Tray -> E.ErrorT R.Error IO Tray
open f t = do
  (n, b) <- readDb f
  return t { nextId = n
           , buffer = b
           , undos = addToUndos (buffer t) (undos t) }
    
saveAs :: CanonPath -> Tray -> E.ErrorT R.Error IO Tray
saveAs f t = do
  writeDb f (nextId t) (buffer t)
  return t { unsaved = Unsaved False
           , filename = Just f }

save :: Tray -> E.ErrorT R.Error IO Tray
save t = case filename t of
  (Nothing) -> E.throwError R.NoSaveFilename
  (Just f) -> saveAs f t

addToUndos :: Buffer -> Undos -> Undos
addToUndos d = Undos . take maxUndos . (d :) . unUndos

-- | Given a function that combines the old Buffer with the new
-- Buffer, carry out a prepend or append operation. All prepended or
-- appended foods must be assigned new IDs (appendOrPrependPure takes
-- care of the renumbering).
appendOrPrependPure :: (Buffer -> [Food] -> Buffer) -- ^ Combiner
                   -> [Food]   -- ^ Loaded foods
                   -> Tray -- ^ Old tray
                   -> Tray
appendOrPrependPure f fLoaded t = t { undos = newUndo
                                    , nextId = newNextId
                                    , unsaved = Unsaved True
                                    , buffer = fN } where
  (fNumbered, newNextId) = assignIds fLoaded (nextId t)
  newUndo = addToUndos (buffer t) (undos t)
  fN = f (buffer t) fNumbered

appendOrPrepend :: (Buffer -> [Food] -> Buffer)
                   -> CanonPath
                   -> Tray
                   -> E.ErrorT R.Error IO Tray
appendOrPrepend g f t = do
  (_, b) <- readDb f
  return $ appendOrPrependPure g (unBuffer b) t

appendFile :: CanonPath -> Tray -> E.ErrorT R.Error IO Tray
appendFile = appendOrPrepend (\(Buffer l) r -> Buffer $ l ++ r)

prependFile :: CanonPath -> Tray -> E.ErrorT R.Error IO Tray
prependFile = appendOrPrepend (\(Buffer l) r -> Buffer $ r ++ l)

close :: Tray -> Tray
close t = (blankTray (clientCurrDir t)) {
  undos = addToUndos (buffer t) (undos t)
  , volatile = volatile t
  , done = done t
  , output = output t }

quit :: Tray -> Tray
quit t = t { done = Done }

------------------------------------------------------------
-- COMPACTION
------------------------------------------------------------

-- | Compacts the Tray so it (hopefully) uses less memory.
-- Does not compact everything in the tray but it comes
-- close.
compact :: Tray -> IO ()
compact = evaluate . Pantry.Tray.compact

------------------------------------------------------------
-- HELP
------------------------------------------------------------
help :: ReportOpts -> Tray -> Tray
help o = report o R.help

version :: ReportOpts -> Tray -> Tray
version o = report o R.version

copyright :: ReportOpts -> Tray -> Tray
copyright o = report o R.copyright

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

partitionSingletons ::
  [[a]]
  -> ([[a]], [a])
partitionSingletons = foldr f ([], []) where
  f (a:[]) (ms, ss) = (ms, a:ss)
  f a (ms, ss) = (a:ms, ss)

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
compose = foldl (flip (.)) id

liftToErrorT :: (E.Error e, Monad m) => Either e a -> E.ErrorT e m a
liftToErrorT e = case e of
  (Left err) -> E.throwError err
  (Right good) -> return good

impurify :: Monad m => (a -> b) -> a -> m b
impurify f a = return $ f a

