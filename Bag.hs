{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bag where

import Food(Food, FoodId, oneFoodId)
import Data.Serialize(Serialize)
import Types(Next)

newtype NextId = NextId { unNextId :: FoodId }
               deriving (Eq, Ord, Next, Serialize)
newtype Filename = Filename  { unFilename :: String }
                   deriving (Show, Serialize)
newtype Unsaved = Unsaved {unUnsaved :: Bool }

newtype Undos = Undos { unUndos :: [Buffer] }
newtype Buffer = Buffer { unBuffer :: [Food] } deriving Serialize

data Bag = Bag { nextId :: NextId
               , filename :: Maybe Filename
               , unsaved :: Unsaved
               , buffer :: Buffer
               , undos :: Undos }

emptyBag :: Bag
emptyBag = Bag { nextId = NextId oneFoodId
               , filename = Nothing
               , unsaved = Unsaved False
               , buffer = Buffer []
               , undos = Undos [] }