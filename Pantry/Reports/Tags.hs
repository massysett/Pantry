module Pantry.Reports.Tags(Pantry.Reports.Tags.tags) where

import qualified Pantry.Food as F
import Pantry.Reports.Types
import Pantry.Reports.Columns ( newspaper )
import Data.Text(append, pack)
import Data.Maybe ( catMaybes )
import qualified Data.Map as M
import Data.List ((\\))
import Prelude hiding (lookup)
import Data.Text(Text)

allTags :: F.Food -> [(F.TagName, F.TagVal)]
allTags = M.assocs . F.getTags

orderedTags :: [F.TagName] -- ^ Tags to show, in order
               -> F.Food
               -> [(F.TagName, F.TagVal)]
orderedTags ns f = catMaybes . map toMaybe $ ns where
  m = F.getTags f
  toMaybe n = do
    v <- M.lookup n m
    return (n, v)

removeRedundantTags :: [(F.TagName, F.TagVal)] -- ^ Tags that must be shown
                       -> [(F.TagName, F.TagVal)] -- ^ All tags
                       -> [(F.TagName, F.TagVal)] -- ^ All tags, with redundant ones removed
removeRedundantTags = flip (\\)

tagsToShow :: Bool      -- ^ True to show all tags,
                        -- False to show ordered tags only
              -> [F.TagName] -- ^ Tag names to show
              -> F.Food
              -> [(F.TagName, F.TagVal)]
tagsToShow showAll ns f
  | showAll = ordered ++ rest
  | null ns = rest
  | otherwise = ordered where
    ordered = orderedTags ns f
    rest = removeRedundantTags ordered (allTags f)

tags :: ReportOpts -> F.Food -> Text
tags o f = newspaper cs ss where
  cs = if oneColumn o then [] else [35]
  ss = map toString ts
  toString ((F.TagName n), (F.TagVal v)) =
    n `append` (pack ": ") `append` v
  ts = tagsToShow (showAllTags o) (showTags o) f
    
