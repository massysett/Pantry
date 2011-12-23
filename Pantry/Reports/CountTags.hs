module Pantry.Reports.CountTags (countTags) where

import Prelude(Int, Ord, succ, show,
               ($), (.), Maybe(Just, Nothing), flip,
               zip, (==), Bool(True, False))
import Data.Maybe (catMaybes)
import Data.List (deleteFirstsBy)
import qualified Pantry.Food as F
import Data.Map(Map, findWithDefault, insertWith, empty, insert,
                assocs, elems, lookup)
import Pantry.Reports.Types(showAllTags, showTags, ReportOpts)
import Data.Text(Text, pack, replicate, singleton,
                 append, snoc, concat)
import qualified Data.Text as X
import Data.Foldable(Foldable, foldr, sum)
import Data.Functor(fmap)

countTags :: (Foldable f) => ReportOpts -> f F.Food -> Text
countTags o = count (showAllTags o) (showTags o)

type ValMap = Map F.TagVal Int
type NameMap = Map F.TagName ValMap

countValue :: (F.TagName, F.TagVal) -> NameMap -> NameMap
countValue (k, v) old = insert k vm old where
  vmOld = findWithDefault empty k old
  vm = insertWith f v 1 vmOld
  f _ o = succ o

countFood :: F.Food -> NameMap -> NameMap
countFood f ns = foldr countValue ns (assocs . F.getTags $ f)

nameMap :: (Foldable f) => f F.Food -> NameMap
nameMap = foldr countFood empty

showValPair :: (F.TagVal, Int) -> Text
showValPair ((F.TagVal t), i) = ldr `append` num `snoc` ' ' `append` txt where
  ldr = replicate 5 (singleton ' ')
  num = pack . show $ i
  txt = t `snoc` '\n'

showMapPair :: (F.TagName, Maybe ValMap) -> Text
showMapPair ((F.TagName n), m) = fir `append` rest `snoc` '\n' where
  fir = num `snoc` ' ' `append` n `snoc` '\n' where
    num = case m of
      Nothing -> singleton '0'
      (Just map) -> pack . show . sum . elems $ map
  rest = case m of
    Nothing -> X.empty
    (Just map) -> concat . fmap showValPair . assocs $ map

-- | Given a list of keys in a map, pull a list of values, in the same
-- order as the original list. Pulls (Just v) for values in the map,
-- or Nothing for values not in the map.
orderedKeys :: (Ord k) => Map k v -> [k] -> [Maybe v]
orderedKeys m = fmap (flip lookup m)

-- | Returns a tuple. The first value is the list of (Key, Maybe
-- value) pairs for the list of keys that was passed in, in order. The
-- second value is the list of "leftover" keys and values--those whose
-- keys were not in the original list.
orderedWithLeftovers :: (Ord k) =>
                        Map k v
                        -> [k]
                        -> ([(k, Maybe v)], [(k, v)])
orderedWithLeftovers m ks = (orig, left) where
  orig = zip ks (orderedKeys m ks)
  left = deleteFirstsBy f (assocs m) origVals where
    f (k1, _) (k2, _) = k1 == k2
    origVals = catMaybes . fmap sndToVal $ orig where
      sndToVal (k, (Just v)) = Just (k, v)
      sndToVal (_, Nothing) = Nothing

count :: (Foldable f) => 
         Bool      -- ^ If true, show all tags. If false, only show all
                   -- tags if next list is empty.
         -> [F.TagName] -- ^ Tags to show
         -> f F.Food -- ^ Foods whose tags to show
         -> Text
count a ns fs = firsts `append` rests where
  nm = nameMap fs
  (fPairs, rPairs) = orderedWithLeftovers nm ns
  firsts = concat . fmap showMapPair $ fPairs
  restTxt = concat . fmap showMapPair
            . fmap (\(k, v) -> (k, Just v)) $ rPairs
  rests = case a of
    True -> restTxt
    False -> case ns of
      [] -> restTxt
      _ -> X.empty
