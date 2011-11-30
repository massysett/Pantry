module Db where

import Data.Sequence(Seq)
import Data.Foldable(Foldable)

import Food(Food, Error)

newtype NextId = NextId { unId :: Integer }
newtype Filename = Filename  { unFilename :: String }
newtype Foods = Foods { unFoods :: Seq Food }
newtype Unsaved = Unsaved {unUnsaved :: Bool }

data Db = Db { dbNextId :: NextId
             , dbFilename :: Filename
             , dbUnsaved :: Unsaved
             , dbFoods :: Foods }

prepend :: Foldable f => f Food -> Db -> Db
prepend = undefined

append :: Foldable f => f Food -> Db -> Db
append = undefined


