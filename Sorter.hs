module Sorter where

import Food

import Data.Map as M

type ValueMap = M.Map String Int
type TagMap = M.Map String ValueMap

addValue :: String -> ValueMap -> ValueMap
addValue s old = case member s old of
  True -> old
  False -> M.insert s (size old) old

addTag :: TagNameVal -> TagMap -> TagMap
addTag t@(TagNameVal (Name n) (TagVal v)) m = M.insert n vm' m where
  vm' = addValue v vmOld
  vmOld = M.findWithDefault M.empty n m

data Direction = Descending | Ascending
data Key = Key Name Direction

pred :: [Key] -> Food -> Food -> Ordering
pred = undefined

keyPred :: TagMap -> Key -> Food -> Food -> Ordering
keyPred ts (Key n d) 
