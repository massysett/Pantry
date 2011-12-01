module Db where

import Prelude(undefined, Either, ($), (.), id, Monad,
               (>>=), return, String, Bool, Maybe, IO,
               Ordering, Integer, flip)
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Applicative(Applicative, (<*>), pure)
import qualified Control.Monad.Error as E (ErrorT)
import qualified Data.Traversable as T
import qualified Data.Text as X

import Food(Food, Error, FoodId, Xform)

newtype NextId = NextId { unNextId :: FoodId }
newtype Filename = Filename  { unFilename :: String }
newtype Foods = Foods { unFoods :: S.Seq Food }
newtype Unsaved = Unsaved {unUnsaved :: Bool }

data Db = Db { dbNextId :: NextId
             , dbFilename :: Maybe Filename
             , dbUnsaved :: Unsaved
             , dbFoods :: Foods }

prepend :: Foods -> Db -> Db
prepend = undefined

append :: Foods -> Db -> Db
append = undefined

select :: (Food -> Bool) -> Db -> Foods
select f = Foods . S.filter f . unFoods . dbFoods

save :: Db -> E.ErrorT Error IO Db
save = undefined

saveAs :: Filename -> Db -> E.ErrorT Error IO Db
saveAs = undefined

load :: Filename -> E.ErrorT Error IO Db
load = undefined

status :: Db -> X.Text
status = undefined

data MoveId = First | Pos FoodId

move :: MoveId -- ^ First one
        -> FoodId -- ^ Second one
        -> [FoodId] -- ^ Subsequent foods
        -> Db
        -> Either Error Db
move = undefined

xform :: Xform -> Db -> Either Error Db
xform f d = do
  newSeq <- T.mapM f . unFoods . dbFoods $ d
  return $ d { dbFoods = Foods newSeq }

addAsIngredients :: Foods -> Db -> Either Error Db
addAsIngredients = undefined

sortDb :: (Food -> Food -> Ordering) -> Db -> Db
sortDb c d = d { dbFoods = newFoods } where
  oldFoods = unFoods . dbFoods $ d
  newFoods = Foods $ S.sortBy c oldFoods

composePreds :: (F.Foldable f, Applicative f)
                => f (a -> Bool)
                -> a
                -> Bool
composePreds ps a = F.and $ ps <*> pure a

edit :: (Food -> Bool) -> Xform -> Db -> Either Error Db
edit = undefined

delete :: (Food -> Bool) -> Db -> Db
delete = undefined

exportIngr :: Foods -> Db -> Db
exportIngr = undefined

-- TODO define NonNegInt
head :: Integer -> Foods -> Foods
head = undefined

-- TODO define NonNegInt
tail :: Integer -> Foods -> Foods
tail = undefined

compose :: F.Foldable f => f (a -> a) -> a -> a
compose = F.foldl (flip (.)) id

composeM :: (F.Foldable f, Monad m)
            => f (a -> m a)
            -> a
            -> m a
composeM fs a = F.foldl (>>=) (return a) fs
