{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pantry.Bag where

import Pantry.Food(Food, FoodId, oneFoodId)
import Data.Serialize(Serialize)
import Pantry.Types(Next)
import Pantry.Paths(CanonPath)

newtype NextId = NextId { unNextId :: FoodId }
               deriving (Eq, Ord, Next, Serialize, Show)
newtype Unsaved = Unsaved {unUnsaved :: Bool }

newtype Undos = Undos { unUndos :: [Buffer] }
newtype Buffer = Buffer { unBuffer :: [Food] } deriving Serialize

data Bag = Bag { nextId :: NextId
               , filename :: Maybe CanonPath
               , unsaved :: Unsaved
               , buffer :: Buffer
               , undos :: Undos }

emptyBag :: Bag
emptyBag = Bag { nextId = NextId oneFoodId
               , filename = Nothing
               , unsaved = Unsaved False
               , buffer = Buffer []
               , undos = Undos [] }
