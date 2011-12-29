{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pantry.Bag where

import Pantry.Food(Food, FoodId, oneFoodId)
import Data.Serialize(Serialize)
import Pantry.Types(Next)
import Pantry.Paths(CanonPath)
import Pantry.Exact ( Exact )
import Control.DeepSeq ( NFData(rnf), deepseq )

newtype NextId = NextId { unNextId :: FoodId }
               deriving (Eq, Ord, Next, Serialize, Show, Exact,
                         NFData)
newtype Unsaved = Unsaved {unUnsaved :: Bool }
                  deriving NFData

newtype Undos = Undos { unUndos :: [Buffer] }
                deriving NFData

newtype Buffer = Buffer { unBuffer :: [Food] }
               deriving (Serialize, NFData)

data Bag = Bag { nextId :: NextId
               , filename :: Maybe CanonPath
               , unsaved :: Unsaved
               , buffer :: Buffer
               , undos :: Undos }

instance NFData Bag where
  rnf b = nextId b `deepseq`
          filename b `deepseq`
          unsaved b `deepseq`
          buffer b `deepseq`
          undos b `deepseq`
          ()

emptyBag :: Bag
emptyBag = Bag { nextId = NextId oneFoodId
               , filename = Nothing
               , unsaved = Unsaved False
               , buffer = Buffer []
               , undos = Undos [] }
