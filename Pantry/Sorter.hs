-- | Sorting foods. For foods to be sorted, they have to be sorted by
-- tag values. foodcmp will compare two foods depending upon
-- particular tags. By default the tags are sorted by a simple
-- lexicographical sort; however, the user can also provide values so
-- that foods are sorted in a particular order (for example you might
-- want to sort a meal tag into breakfast, lunch, and dinner or some
-- such).
module Pantry.Sorter( foodcmp
             , Key(..)
             , Direction(..)
             , TagMap
             , ValueMap
             , addTag
             ) where

import Pantry.Food

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Foldable as F
import Pantry.Types(Name)

-- | How to sort particular tag values in order. For instance, if the
-- user wants an ascending sort of the @meal@ tag to be @breakfast@,
-- @lunch@, @dinner@, then this map should look something like
-- @M.fromList [(\"breakfast\", 0), (\"lunch\", 1), (\"dinner\",
-- 2)]@. The ValueMap is placed as values in a TagMap.
type ValueMap = M.Map TagVal Int

-- | Holds a key-value pair for each tag that the user wants to be
-- sorted in a particular order. The key is the name of the tag; the
-- value is the corresponding ValueMap. If there is no pair in the map
-- for a particular tag, then that tag is sorted in lexicographical
-- order. For instance, to sort the @meal" tag in a certain order (see
-- example under documentation for ValueMap) then put a value in this
-- map.
type TagMap = M.Map Name ValueMap

-- | Add a value to a ValueMap, properly incrementing the count of the
-- value.
addValue :: TagVal -> ValueMap -> ValueMap
addValue s old = case M.member s old of
  True -> old
  False -> M.insert s (M.size old) old

-- | Adds a tag name and value pair to a TagMap. Can be used inside of
-- a foldr.
addTag :: TagNameVal -> TagMap -> TagMap
addTag (TagNameVal n v) m = M.insert n vm' m where
  vm' = addValue v vmOld
  vmOld = M.findWithDefault M.empty n m

-- | The direction to sort tag values in--ascending or descending order.
data Direction = Descending | Ascending

-- | Indicates that foods should be ordered based on the values of a
-- particular direction. For instance, to sort foods by the @name@ tag
-- in ascending order, supply an appropriate Key. The order of the Key
-- items is significant; foods are first ordered based on the first
-- key; the second key is consulted only if the foods are equal
-- according to the first key; etc.
data Key = Key Name Direction

-- | Compares foods based on a particular TagMap and a series of Key
-- items. Foods are compared only based upon the Key items; having
-- items in the TagMap is not significant if there is no corresponding
-- Key item. foodcmp works by first consulting the tag values for the
-- tag name given in the Key. The two foods are compared based on the
-- tag value. For an ascending sort:
--
-- * if the two foods both have the tag, AND the tag name is in the
-- TagMap, and BOTH tag values are in the corresponding ValueMap, then
-- the foods are compared based on what's in the ValueMap. The food
-- whose value in the ValueMap is higher is returned as greater.
--
-- * if the two foods both have the tag, AND the tag name is in the
-- TagMap, but only ONE tag value is in the corresponding ValueMap,
-- then the food whose value is IN the ValueMap is returned as LESS
-- THAN the other. (This sorts the foods whose values are in the
-- ValueMap first; presumably the user is more interested in these.)
--
-- * If the two foods both have the tag, AND the tag name is NOT in
-- the TagMap, OR if the tag name is in the value map but NEITHER
-- food's tag value is in the ValueMap, then returns the result of
-- compare (tag value 1) (tag value 2).
--
-- * If only ONE food has the tag, then that food is returned at being
-- LESS THAN the other food--again, to sort it to the beginning.
--
-- * If NEITHER food has the tag, then the foods are EQUAL.
--
-- For an ascending sort, reverse all the above results.
--
-- If the result of comparing a single key is either LT or GT, then
-- foodcmp returns that. If the result is EQ, then foodcmp moves on to
-- the next Key (if there is one). This proceeds either until an LT or
-- GT value is obtained or until Key items are exhausted; then the
-- last value computed is returned.
foodcmp :: (F.Foldable f) => TagMap -> f Key -> Food -> Food -> Ordering
foodcmp ts ks x y = F.foldl (cmpNewKey ts x y) EQ ks

-- | Compares two foods based upon the results of a previous
-- comparison and a particular key. If the previous ordering was EQ,
-- then compare these two foods based on this key. Otherwise, if the
-- previous ordering returned LT or GT, then simply return that.
cmpNewKey :: TagMap -> Food -> Food -> Ordering -> Key -> Ordering
cmpNewKey ts x y EQ k = keyPred ts k x y
cmpNewKey _  _ _  o _ = o

-- | Compares two foods based on the value of their tags that
-- corespond to a particular Key. See "foodcmp" for details. Note that
-- this function is not necessarily consulted for every Key for every
-- Food, as the first Key that yields an LT or GT value will in effect
-- have the last word.
keyPred :: TagMap -> Key -> Food -> Food -> Ordering
keyPred ts (Key n d) x y = rev r d where
  rev LT Descending = GT
  rev GT Descending = LT
  rev o _ = o
  get = getTag n
  r = case (get x, get y) of
    ((Just _), Nothing) -> LT
    (Nothing, (Just _)) -> GT
    (Nothing, Nothing) -> EQ
    ((Just (TagNameVal _ vx)), (Just (TagNameVal _ vy))) ->
      case M.lookup n ts of
        Nothing -> compare vx vy
        (Just vm) -> case (M.lookup vx vm, M.lookup vy vm) of
          ((Just _), Nothing) -> LT
          (Nothing, (Just _)) -> GT
          (Nothing, Nothing) -> EQ
          ((Just ix), (Just iy)) -> compare ix iy
