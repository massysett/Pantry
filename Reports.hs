module Reports where

import Prelude(Ord, Either(Left, Right), Maybe(Just, Nothing),
               ($), fst, snd, map, filter, zip, (.), String,
               undefined, (++), return)
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
import Reports.Types(FoodRpt, TotalRpt, ReportOpts)
import Food(Food, foldFoodNuts, Error(NoReportMatch),
            NutNamesAmts)
import qualified Data.Text as X
import qualified Data.Foldable as F
import qualified Control.Monad.Error as E

import Db(Db)
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

foodRpts :: [(String, FoodRpt)]
foodRpts = [
  ("blank", (\_ _ _ -> blank))
  , ("ingredients", (\_ _ -> ingredients))
  , ("name", (\_ _ -> name))
  , ("nutrients", nuts)
  , ("paste", (\_ _ -> paste))
  , ("properties", (\o _ f -> properties o f))
  , ("tags", (\o _ f -> tags o f))
  , ("units", (\_ _ -> units))
  ]

totalRpts :: (F.Foldable f) => [(String, TotalRpt f)]
totalRpts = [
  ("count-tags", (\o _ _ fs -> countTags o fs))
  , ("total", (\o ts _ _ -> total o ts))
  ]

printRpts :: (F.Foldable f)
             => ReportGroups f
             -> f Food
             -> Db
             -> X.Text
printRpts = undefined

printFoodRpts :: (Applicative f, F.Foldable f)
                 => ReportOpts
                 -> NutNamesAmts
                 -> f Food
                 -> f FoodRpt
                 -> X.Text
printFoodRpts o ts fs rs = concat xs where
  xs = rs <*> pure o <*> pure ts <*> fs

printTotalRpts :: (Applicative f, F.Foldable f)
                  => ReportOpts
                  -> NutNamesAmts
                  -> Db
                  -> f Food
                  -> f (TotalRpt f)
                  -> X.Text
printTotalRpts o ts d fs rs = concat xs where
  xs = rs <*> pure o <*> pure ts <*> pure d <*> pure fs

{-
printEitherRpt :: (Applicative f, F.Foldable f)
                  => ReportOpts
                  -> NutNamesAmts
                  -> Db
                  -> f Food
                  -> 
-}
data ReportGroups f = ReportGroups [Either (f FoodRpt) (f (TotalRpt f))]

buildReportGroups :: (F.Foldable f) =>
                     [String] -> Either Error (ReportGroups f)
buildReportGroups = F.foldrM addReport (ReportGroups [])

addReport :: (F.Foldable f)
             => String -> ReportGroups f -> Either Error (ReportGroups f)
addReport s r@(ReportGroups rs) = do
  ei <- bestReport s
  return $ ReportGroups $ case rs of
    [] -> case ei of
      (Left f) -> [(Left [f])]
      (Right t) -> [(Right [t])]
    l@((Left ls):rs) -> case ei of
      (Left f) -> (Left (f:ls)):rs
      (Right t) -> Right [t]:l
    l@((Right ls):rs) -> case ei of
      (Left f) -> Left [f]:l
      (Right t) -> (Right (t:ls)):rs

bestReport :: (F.Foldable f) =>
              String -> Either Error (Either FoodRpt (TotalRpt f))
bestReport s = case bestMatch s (M.fromList foodRpts) of
  (Right r) -> Right . Left $ r
  (Left fms) -> case bestMatch s (M.fromList totalRpts) of
    (Right r) -> Right . Right $ r
    (Left tms) -> E.throwError $ NoReportMatch s (fms ++ tms)
  

concat :: (F.Foldable f) => f X.Text -> X.Text
concat = F.foldr X.append X.empty
