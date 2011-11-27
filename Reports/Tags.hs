module Reports.Tags(Reports.Tags.tags) where

import Food
import Reports.Types
import Reports.Columns
import Data.Text(append, pack)
import Data.Maybe
import Data.Map hiding (map, null, (\\))
import Data.List hiding (lookup)
import Prelude hiding (lookup)

allTags :: Food -> [(Name, TagVal)]
allTags f = assocs m where
  (TagNamesVals m) = Food.tags f

orderedTags :: [Name] -- ^ Tags to show, in order
               -> Food
               -> [(Name, TagVal)]
orderedTags ns f = catMaybes . map toMaybe $ ns where
  (TagNamesVals m) = Food.tags f
  toMaybe n = do
    v <- lookup n m
    return (n, v)

removeRedundantTags :: [(Name, TagVal)] -- ^ Tags that must be shown
                       -> [(Name, TagVal)] -- ^ All tags
                       -> [(Name, TagVal)] -- ^ All tags, with redundant ones removed
removeRedundantTags = flip (\\)

tagsToShow :: Bool      -- ^ True to show all tags,
                        -- False to show ordered tags only
              -> [Name] -- ^ Tag names to show
              -> Food
              -> [(Name, TagVal)]
tagsToShow showAll ns f
  | showAll = ordered ++ rest
  | null ns = rest
  | otherwise = ordered where
    ordered = orderedTags ns f
    rest = removeRedundantTags ordered (allTags f)

tags :: Report
tags = emptyRpt {body = b} where
  b o _ f = newspaper cs ss where
    cs = if oneColumn o then [] else [35]
    ss = map toString ts
    toString ((Name n), (TagVal v)) =
      n `append` (pack ": ") `append` v
    ts = tagsToShow (showAllTags o) (showTags o) f
    
