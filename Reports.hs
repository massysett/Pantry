module Reports where

import Prelude(Ord, Either(Left, Right), Maybe(Just, Nothing),
               ($), fst, snd, map, filter, zip, (.), String)
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
import Control.Applicative((<$>), (<*>), pure, Applicative)
import qualified Data.Foldable as F

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

reports :: (F.Foldable f) => M.Map String (Report f)
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

bestReport :: (F.Foldable f) => String -> Either [String] (Report f)
bestReport s = bestMatch s reports

runReports :: (F.Foldable f, Applicative f) =>
              f (Report f)
              -> ReportOpts
              -> f Food
              -> X.Text
runReports rs o fs = h `X.append` b `X.append` f where
  h = concat $ header <$> rs <*> pure o <*> pure t <*> pure fs
  b = concat $ body <$> rs <*> pure o <*> pure t <*> fs
  f = concat $ footer <$> rs <*> pure o <*> pure t <*> pure fs
  t = foldFoodNuts fs

concat :: (F.Foldable f) => f X.Text -> X.Text
concat = F.foldr X.append X.empty
