module Pantry.Sorter( foodcmp
             , Key
             , TagMap
             , addTag
             , makeTagMap
             ) where

import Pantry.Food

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Foldable as F

type ValueMap = M.Map TagVal Int
type TagMap = M.Map Name ValueMap

addValue :: TagVal -> ValueMap -> ValueMap
addValue s old = case M.member s old of
  True -> old
  False -> M.insert s (M.size old) old

addTag :: TagNameVal -> TagMap -> TagMap
addTag t@(TagNameVal n v) m = M.insert n vm' m where
  vm' = addValue v vmOld
  vmOld = M.findWithDefault M.empty n m

makeTagMap :: (F.Foldable f) => f TagNameVal -> TagMap
makeTagMap = F.foldr addTag M.empty

data Direction = Descending | Ascending
data Key = Key Name Direction

foodcmp :: (F.Foldable f) => TagMap -> f Key -> Food -> Food -> Ordering
foodcmp ts ks x y = F.foldr (cmpNewKey ts x y) EQ ks

cmpNewKey :: TagMap -> Food -> Food -> Key -> Ordering -> Ordering
cmpNewKey ts x y k EQ = keyPred ts k x y
cmpNewKey _  _ _  _ o = o

keyPred :: TagMap -> Key -> Food -> Food -> Ordering
keyPred ts (Key n d) x y = rev r d where
  rev LT Descending = GT
  rev GT Descending = LT
  rev o _ = o
  has = hasTag n
  r
    | has x && (not . has) y = LT
    | (not . has) x && has y = GT
    | (not . has) x && (not . has) y = EQ
    | otherwise = cmpTag ts n x y

-- | Compares two tags, when both foods already have the tag. Do not
-- pass foods that do not both have the tag to this function, or a
-- crash will result.
cmpTag :: TagMap -> Name -> Food -> Food -> Ordering
cmpTag ts n x y
  | M.notMember n ts = compare tx ty
  | M.member tx vm && (not . M.member ty) vm = LT
  | (not . M.member tx) vm && M.member ty vm = GT
  | otherwise = compare vmX vmY
    where
      tx = toVal . fromJust . (getTag n) $ x
      ty = toVal . fromJust . (getTag n) $ y
      vmX = fromJust . (M.lookup tx) $ vm
      vmY = fromJust . (M.lookup ty) $ vm
      vm = fromJust . (M.lookup n) $ ts
      toVal = \(TagNameVal n v) -> v
