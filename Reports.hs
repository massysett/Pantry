module Reports where

import Reports.Blank(blank)
import Reports.CountTags(countTags)
import Reports.Ingredients(ingredients)
import Reports.Name(name)
import Reports.Nuts(nuts)
import Reports.Paste(paste)
import Reports.Properties(properties)
import Reports.Tags(tags)
import Reports.Total(total)
import Reports.Units(units)
import Reports.Types(Report, ReportOpts, header, body, footer)
import Food(Food, foldFoodNuts)
import qualified Data.Text as X

import Data.List(isPrefixOf)
import qualified Data.Map as M
import Control.Applicative

-- | Given a string, and a map with string keys, find the best match
-- for the input string. If there is one exact match, use
-- that. Otherwise, if there is a single match that starts with the
-- input string, use that. Otherwise, return a Left with the matches
-- (might be zero, might be two or more.)

bestMatch :: Ord a => [a] -> M.Map [a] b -> Either [[a]] b
bestMatch k m = case M.lookup k m of
  (Just v) -> Right v
  Nothing -> case ms of
      x:[] -> Right $ snd x
      x -> Left (map fst x)
  where
    ms = map snd
         . filter fst
         . zip (map (k `isPrefixOf`) (M.keys m))
         . M.assocs
         $ m

reports :: M.Map String Report
reports = M.fromList $ [
  ("blank", blank)
  , ("count-tags", countTags)
  , ("ingredients", ingredients)
  , ("name", name)
  , ("nutrients", nuts)  
  , ("paste", paste)
  , ("properties", properties)
  , ("tags", tags)
  , ("total", total)
  , ("units", units)
  ]

bestReport :: String -> Either [String] Report
bestReport s = bestMatch s reports

runReports :: [Report] -> ReportOpts -> [Food] -> X.Text
runReports rs o fs = h `X.append` b `X.append` f where
  h = X.concat $ header <$> rs <*> pure o <*> pure t <*> pure fs
  b = X.concat $ body <$> rs <*> pure o <*> pure t <*> fs
  f = X.concat $ footer <$> rs <*> pure o <*> pure t <*> pure fs
  t = foldFoodNuts fs
