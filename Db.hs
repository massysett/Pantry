module Db where

import Prelude(undefined, Either, ($), (.), id, Monad,
               (>>=), return, String, Bool, Maybe, IO,
               Ordering, Integer, flip, Either(Left, Right))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Applicative(Applicative, (<*>), pure)
import qualified Control.Monad.Error as E
import qualified Data.Traversable as T
import qualified Data.Text as X
import qualified Control.Monad.Writer as W
import qualified Data.DList as DL
import Types(NonNegInteger, PosInteger)

import Food(Food, Error, FoodId, Xform)

newtype NextId = NextId { unNextId :: FoodId }
newtype Filename = Filename  { unFilename :: String }
newtype Foods = Foods { unFoods :: S.Seq Food }
newtype Unsaved = Unsaved {unUnsaved :: Bool }

data Db = Db { dbNextId :: NextId
             , dbFilename :: Maybe Filename
             , dbUnsaved :: Unsaved
             , dbFoods :: Foods }

data Tray = Tray { trayDb :: Db
                 , volatile :: [Food] 
                 , done :: Done
                 , output :: DL.DList X.Text }

type Convey = Tray -> E.ErrorT Error IO Tray

blankDb :: Db
blankDb = undefined

data Done = Done | NotDone

xform :: Xform -> Db -> Either Error Db
xform f d = do
  newSeq <- T.mapM f . unFoods . dbFoods $ d
  return $ d { dbFoods = Foods newSeq }

sortDb :: (Food -> Food -> Ordering) -> Db -> Db
sortDb c d = d { dbFoods = newFoods } where
  oldFoods = unFoods . dbFoods $ d
  newFoods = Foods $ S.sortBy c oldFoods

composePreds :: (F.Foldable f, Applicative f)
                => f (a -> Bool)
                -> a
                -> Bool
composePreds ps a = F.and $ ps <*> pure a

liftToErrorT :: (E.Error e, Monad m) => Either e a -> E.ErrorT e m a
liftToErrorT e = case e of
  (Left err) -> E.throwError err
  (Right good) -> return good
