module Pantry.Reports (ReportGroups, buildReportGroups,
                       printReportGroups, help, copyright,
                       version) where

import Pantry.Reports.Blank(blank)
import qualified Pantry.Reports.Copyright(copyright)
import Pantry.Reports.CountTags(countTags)
import qualified Pantry.Reports.Help(help)
import Pantry.Reports.Ingredients(ingredients)
import Pantry.Reports.Name(name)
import Pantry.Reports.Nuts(nuts)
import Pantry.Reports.Paste(paste)
import Pantry.Reports.Properties(properties)
import Pantry.Reports.Status(status)
import Pantry.Reports.Tags(tags)
import Pantry.Reports.Total(total)
import Pantry.Reports.Units(units)
import Pantry.Reports.Types(FoodRpt, TotalRpt, ReportOpts)
import qualified Pantry.Reports.Version(version)
import Pantry.Food(Food, sumNuts, NutName, NutAmt, getNuts)
import qualified Pantry.Error as R
import qualified Data.Text as X
import qualified Data.Foldable as F
import qualified Control.Monad.Error as E
import qualified Data.DList as DL
import Data.List ( transpose )

import qualified Pantry.Tray as T
import Data.Text ( Text, isPrefixOf, pack )
import qualified Data.Map as M
import Control.Applicative((<*>), pure, Applicative)

-- | Given a string, and a map with string keys, find the best match
-- for the input string. If there is one exact match, use
-- that. Otherwise, if there is a single match that starts with the
-- input string, use that. Otherwise, return a Left with the matches
-- (might be zero, might be two or more.)

bestMatch :: Text -> M.Map Text b -> Either [Text] b
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

foodRpts :: [(Text, FoodRpt)]
foodRpts = map (\(s, r) -> (pack s, r)) [
  ("blank", (\_ _ _ -> blank))
  , ("ingredients", (\_ _ -> ingredients))
  , ("name", (\_ _ -> name))
  , ("nutrients", nuts)
  , ("paste", (\_ _ -> paste))
  , ("properties", (\o _ f -> properties o f))
  , ("tags", (\o _ f -> tags o f))
  , ("units", (\_ _ -> units))
  ]

totalRpts :: [(Text, TotalRpt)]
totalRpts = map (\(s, r) -> (pack s, r)) [
  ("copyright", (\_ _ _ _ -> Pantry.Reports.Copyright.copyright))
  , ("count-tags", (\o _ _ fs -> countTags o fs))
  , ("help", (\_ _ _ _ -> Pantry.Reports.Help.help))
  , ("status", (\_ _ t _ -> status t))
  , ("total", (\o ts _ _ -> total o ts))
  , ("version", (\_ _ _ _ -> Pantry.Reports.Version.version))
  ]

printFoodRpts :: ReportOpts
                 -> M.Map NutName NutAmt
                 -> [Food]
                 -> [FoodRpt]
                 -> DL.DList X.Text
printFoodRpts o ts fs rs = let
  f report done = map report fs : done
  in DL.fromList
     . concat
     . transpose
     . foldr f []
     $ rs <*> pure o <*> pure ts

printTotalRpts :: ReportOpts
                  -> M.Map NutName NutAmt
                  -> T.Tray
                  -> [Food]
                  -> [TotalRpt]
                  -> DL.DList X.Text
printTotalRpts o ts tr fs rs = DL.fromList xs where
  xs = rs <*> pure o <*> pure ts <*> pure tr <*> pure fs


printEitherRpt :: ReportOpts
                  -> M.Map NutName NutAmt
                  -> T.Tray
                  -> Either [FoodRpt] [TotalRpt]
                  -> DL.DList X.Text
printEitherRpt o ts tr ei =
  let fs = T.unVolatile . T.volatile $ tr
  in case ei of
    (Left fr) -> printFoodRpts o ts fs fr
    (Right tot) -> printTotalRpts o ts tr fs tot

printReportGroups :: ReportOpts
                     -> ReportGroups
                     -> T.Tray
                     -> DL.DList X.Text
printReportGroups o (ReportGroups ls) t = r where
  r = DL.concat . map (printEitherRpt o ts t) $ ls
  ts = foldr sumNuts M.empty
       . map getNuts
       . T.unVolatile
       . T.volatile $ t

data ReportGroups = ReportGroups [Either [FoodRpt] [TotalRpt]]

help :: ReportGroups
help = ReportGroups [Right [\_ _ _ _ -> Pantry.Reports.Help.help]]

version :: ReportGroups
version = ReportGroups [Right l] where
  l = [\_ _ _ _ -> Pantry.Reports.Version.version]

copyright :: ReportGroups
copyright = ReportGroups [Right l] where
  l = [\_ _ _ _ -> Pantry.Reports.Copyright.copyright]

buildReportGroups :: [Text] -> Either R.Error ReportGroups
buildReportGroups = F.foldrM addReport (ReportGroups [])

addReport :: Text -> ReportGroups -> Either R.Error ReportGroups
addReport s (ReportGroups gs) = do
  ei <- bestReport s
  return $ ReportGroups $ case gs of
    [] -> case ei of
      (Left f) -> [(Left [f])]
      (Right t) -> [(Right [t])]
    l@((Left ls):rs) -> case ei of
      (Left f) -> (Left (f:ls)):rs
      (Right t) -> Right [t]:l
    l@((Right ls):rs) -> case ei of
      (Left f) -> Left [f]:l
      (Right t) -> (Right (t:ls)):rs

bestReport :: Text -> Either R.Error (Either FoodRpt TotalRpt)
bestReport s = case bestMatch s (M.fromList foodRpts) of
  (Right r) -> Right . Left $ r
  (Left fms) -> case bestMatch s (M.fromList totalRpts) of
    (Right r) -> Right . Right $ r
    (Left tms) -> E.throwError $ R.NoReportMatch s (fms ++ tms)
