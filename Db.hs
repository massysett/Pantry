{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Food(Food, Error(MoveStartNotFound, MoveIdNotFound,
                        FileReadError, NotPantryFile,
                        WrongFileVersion, FileDecodeError,
                        FileSaveError),
            FoodId, foodId)

newtype NextId = NextId { unNextId :: FoodId }
               deriving (Eq, Ord, Next, Serialize)
newtype Filename = Filename  { unFilename :: String }
                   deriving (Show, Serialize)
newtype Unsaved = Unsaved {unUnsaved :: Bool }

data Db = Db { dbNextId :: NextId
             , dbFilename :: Maybe Filename
             , dbUnsaved :: Unsaved
             , dbFoods :: [Food] }

data Tray = Tray { trayDb :: Db
                 , volatile :: [Food] 
                 , done :: Done
                 , output :: DL.DList X.Text }

type Convey = Tray -> E.ErrorT Error IO Tray

blankDb :: Db
blankDb = undefined

data Done = Done | NotDone

xformToConvey :: (Food -> Either Error Food)
                 -> (Tray -> E.ErrorT Error IO Tray)
xformToConvey f = \t -> do
  let oldDb = trayDb t
      foods = dbFoods oldDb
  newFoods <- liftToErrorT $ T.mapM f foods
  return t { trayDb = oldDb { dbFoods = newFoods } }

predToConvey :: (Food -> Bool)
                -> (Tray -> E.ErrorT Error IO Tray)
predToConvey p = f where 
  f t = return newTray where
    oldDb = trayDb t
    foods = dbFoods oldDb
    newFoods = filter p foods
    newTray = t { trayDb = oldDb { dbFoods = newFoods } }

filterToConvey :: ([Food] -> [Food])
                  -> (Tray -> E.ErrorT Error IO Tray)
filterToConvey f = \t ->
  let oldDb = trayDb t
      newFoods = f $ dbFoods oldDb
  in return $ t { trayDb = oldDb { dbFoods = newFoods } }

newVolatileToConvey :: [Food] -> (Tray -> E.ErrorT Error IO Tray)
newVolatileToConvey fs = \t ->
  let oldDb = trayDb t
      newFoods = fs
  in return $ t { trayDb = oldDb { dbFoods = newFoods } }

-- TODO needs to assign new IDs
append :: Tray -> E.ErrorT Error IO Tray
append t = return newT where
  newT = t { trayDb = oldDb { dbFoods = foods } }
  foods = oldFoods ++ volatile t
  oldFoods = dbFoods oldDb
  oldDb = trayDb t

-- TODO needs to assign new IDs
prepend :: Tray -> E.ErrorT Error IO Tray
prepend t = return newT where
  newT = t { trayDb = oldDb { dbFoods = foods } }
  foods = volatile t ++ oldFoods
  oldFoods = dbFoods oldDb
  oldDb = trayDb t

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
  newT = t { trayDb = oldDb { dbFoods = newFoods } }
  oldDbFoods = dbFoods oldDb
  oldDb = trayDb t
  newFoods = replaceManyFirsts eqId (volatile t) oldDbFoods
  eqId f1 f2 = foodId f1 == foodId f2

delete :: Tray -> E.ErrorT Error IO Tray
delete t = return newT where
  newT = t { trayDb = oldDb { dbFoods = newFoods } }
  oldDbFoods = dbFoods oldDb
  oldDb = trayDb t
  newFoods = deleteManyFirsts eqId (volatile t) oldDbFoods
  eqId f1 f2 = foodId f1 == foodId f2

data FirstPos = Beginning | After FoodId

move :: FirstPos -> [FoodId] -> Tray -> E.ErrorT Error IO Tray
move p is t = do
  let v = volatile t
      pd fid food = fid == foodId food
  fs <- liftToErrorT $ findManyWithFail MoveIdNotFound pd is v
  let sorted = sortByOrder is foodId fs
      deleted = deleteManyFirsts pd is v
  newV <- case p of
    Beginning -> return $ sorted ++ deleted
    (After aft) ->
      let (pre, suf) = L.break (pd aft) deleted
      in case suf of
        [] -> E.throwError $ MoveStartNotFound aft
        _ -> return $ pre ++ sorted ++ suf
  let newTray = t { volatile = newV }
  return newTray

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

-- | Do not canonicalize the input filename. This must happen on the
-- client side.
readDb :: Filename -> E.ErrorT Error IO Db
readDb f = do
  bs <- readBS f
  db <- liftToErrorT $ decodeBSWithHeader f bs
  return db

liftToErrorT :: (E.Error e, Monad m) => Either e a -> E.ErrorT e m a
liftToErrorT e = case e of
  (Left err) -> E.throwError err
  (Right good) -> return good


-- Local Variables:
-- compile-command: "ghc -Wall -outputdir temp Db.hs"
-- End:
