module Reports.Tags where

import Food
import Reports.Types
import Reports.Columns
import Data.Text hiding (map, null)
import Data.Maybe
import Data.Map hiding (map, null, (\\))
import Data.List hiding (lookup)
import Prelude hiding (lookup)
import qualified Data.Text as X

tagRpt :: Report
tagRpt = emptyRpt {body = b} where
  b o _ f = newspaper cs ss where
    cs = if oneColumn o then [] else [35]
    ss = map toString ts
    toString ((Name n), (TagVal v)) =
      n `append` (pack ": ") `append` v
    ts = orderedNV ++ restNV
    (TagNamesVals m) = tags f
    orderedNV = catMaybes . map (lookupPair m) . showTags $ o
    unorderedNV = assocs m \\ orderedNV
    restNV | null (showTags o) || showAllTags o = unorderedNV
           | otherwise = []
    
lookupPair ::(Ord k) => Map k v -> k -> Maybe (k, v)
lookupPair m k = do
  v <- lookup k m
  return (k, v)
    
