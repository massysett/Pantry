module Pantry.Reports where

import Prelude(Ord, Either(Left, Right), Maybe(Just, Nothing),
               ($), fst, snd, map, filter, zip, (.), String,
               undefined, (++), return)
import Pantry.Reports.Blank(blank)
import Pantry.Reports.CountTags(countTags)
import Pantry.Reports.Help(help)
import Pantry.Reports.Ingredients(ingredients)
import Pantry.Reports.Name(name)
import Pantry.Reports.Nuts(nuts)
import Pantry.Reports.Paste(paste)
import Pantry.Reports.Properties(properties)
import Pantry.Reports.Tags(tags)
import Pantry.Reports.Total(total)
import Pantry.Reports.Units(units)
import Pantry.Reports.Types(FoodRpt, TotalRpt, ReportOpts)
import Pantry.Food(Food, foldFoodNuts, Error(NoReportMatch),
            NutNamesAmts)
import qualified Data.Text as X
import qualified Data.Foldable as F
import qualified Control.Monad.Error as E

import Pantry.Tray ( Tray )
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

totalRpts :: [(String, TotalRpt)]
totalRpts = [
  ("count-tags", (\o _ _ fs -> countTags o fs))
  , ("help", (\_ _ _ _ -> help))
  , ("total", (\o ts _ _ -> total o ts))
  ]

printRpts ::ReportGroups
             -> [Food]
             -> Tray
             -> X.Text
printRpts = undefined

printFoodRpts :: ReportOpts
                 -> NutNamesAmts
                 -> [Food]
                 -> [FoodRpt]
                 -> X.Text
printFoodRpts o ts fs rs = X.concat xs where
  xs = rs <*> pure o <*> pure ts <*> fs

printTotalRpts :: ReportOpts
                  -> NutNamesAmts
                  -> Tray
                  -> [Food]
                  -> [TotalRpt]
                  -> X.Text
printTotalRpts o ts tr fs rs = X.concat xs where
  xs = rs <*> pure o <*> pure ts <*> pure tr <*> pure fs


printEitherRpt :: ReportOpts
                  -> NutNamesAmts
                  -> Tray
                  -> [Food]
                  -> Either [FoodRpt] [TotalRpt]
                  -> X.Text
printEitherRpt o ts tr fs ei = case ei of
  (Left fr) -> printFoodRpts o ts fs fr
  (Right tot) -> printTotalRpts o ts tr fs tot

printReportGroups :: ReportOpts
                     -> NutNamesAmts
                     -> Tray
                     -> [Food]
                     -> ReportGroups
                     -> X.Text
printReportGroups o ts tr fs (ReportGroups ls) =
  X.concat . map (printEitherRpt o ts tr fs) $ ls

printReports :: ReportOpts
                -> Tray
                -> [Food] 
                -> [String] 
                -> Either Error X.Text
printReports o tr fs ns = do
  g <- buildReportGroups ns
  let ts = foldFoodNuts fs
  return $ printReportGroups o ts tr fs g

data ReportGroups = ReportGroups [Either [FoodRpt] [TotalRpt]]

buildReportGroups :: [String] -> Either Error ReportGroups
buildReportGroups = F.foldrM addReport (ReportGroups [])

addReport :: String -> ReportGroups -> Either Error ReportGroups
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

bestReport :: String -> Either Error (Either FoodRpt TotalRpt)
bestReport s = case bestMatch s (M.fromList foodRpts) of
  (Right r) -> Right . Left $ r
  (Left fms) -> case bestMatch s (M.fromList totalRpts) of
    (Right r) -> Right . Right $ r
    (Left tms) -> E.throwError $ NoReportMatch s (fms ++ tms)
