module Db where

import Prelude(undefined, Either, ($), (.), id, Monad,
               (>>=), return, String, Bool, Maybe, IO,
               Ordering, Integer, flip, Either(Left, Right),
               filter, (++), (==))
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Applicative(Applicative, (<*>), pure)
import qualified Control.Monad.Error as E
import qualified Data.Traversable as T
import qualified Data.Text as X
import qualified Control.Monad.Writer as W
import qualified Data.DList as DL
import Types(NonNegInteger, PosInteger)

import Food(Food, Error, FoodId, Xform, foodId)

newtype NextId = NextId { unNextId :: FoodId }
newtype Filename = Filename  { unFilename :: String }
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

append :: Tray -> E.ErrorT Error IO Tray
append t = return newT where
  newT = t { trayDb = oldDb { dbFoods = foods } }
  foods = oldFoods ++ volatile t
  oldFoods = dbFoods oldDb
  oldDb = trayDb t

prepend :: Tray -> E.ErrorT Error IO Tray
prepend t = return newT where
  newT = t { trayDb = oldDb { dbFoods = foods } }
  foods = volatile t ++ oldFoods
  oldFoods = dbFoods oldDb
  oldDb = trayDb t

replace :: Tray -> E.ErrorT Error IO Tray
replace t = return newT where
  newT = t { trayDb = oldDb { dbFoods = foods } }
  foods = volatile t
  oldFoods = dbFoods oldDb
  oldDb = trayDb t


edit :: Tray -> E.ErrorT Error IO Tray
edit t = return newT where
  newT = t { trayDb = oldDb { dbFoods = newFoods } }
  oldDbFoods = dbFoods oldDb
  oldDb = trayDb t
  newFoods = replaceAll eqId (volatile t) oldDbFoods
  eqId f1 f2 = foodId f1 == foodId f2

delete :: Tray -> E.ErrorT Error IO Tray
delete t = return newT where
  newT = t { trayDb = oldDb { dbFoods = newFoods } }
  oldDbFoods = dbFoods oldDb
  oldDb = trayDb t
  newFoods = deleteAll eqId (volatile t) oldDbFoods
  eqId f1 f2 = foodId f1 == foodId f2

deleteAll :: (a -> a -> Bool) -- ^ How to make a predicate
             -> [a]           -- ^ Source of what to delete
             -> [a]           -- ^ Target to delete in
             -> [a]           -- ^ Deleted
deleteAll pm rs = composed where
  preds = L.map pm rs
  delFns = L.map deleteFirst preds
  composed = compose delFns

deleteFirst :: (a -> Bool)  -- ^ Predicate
               -> [a]       -- ^ Delete from here
               -> [a]       -- ^ With first match deleted
deleteFirst p ls = case L.break p ls of
  (ns, []) -> ns
  (ns, as) -> ns ++ L.tail as

replaceAll :: (a -> a -> Bool) -- ^ How to make a predicate
              -> [a]             -- ^ Source of replacements
              -> [a]             -- ^ Target to replace within
              -> [a]             -- ^ Replacements
replaceAll pm rs = composed where
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
