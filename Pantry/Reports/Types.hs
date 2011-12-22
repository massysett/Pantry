module Pantry.Reports.Types (GoalNameAmt(GoalNameAmt),
                      ReportOpts(ReportOpts, goals, showAllNuts,
                                 showTags, showAllTags,
                                 oneColumn),
                      FoodRpt, TotalRpt,
                      defaultReportOpts) where

import Prelude(Bool(False))
import Data.Text(Text)
import Pantry.Food(Food, NutNamesAmts, NutAmt, Name)
import Pantry.Tray(Tray)

data GoalNameAmt = GoalNameAmt Name NutAmt

type FoodRpt = ReportOpts -> NutNamesAmts -> Food -> Text
type TotalRpt = ReportOpts -> NutNamesAmts -> Tray -> [Food] -> Text

data ReportOpts = ReportOpts { goals :: [GoalNameAmt]
                             , showAllNuts :: Bool
                             , showTags :: [Name]
                             , showAllTags :: Bool
                             , oneColumn :: Bool }

defaultReportOpts :: ReportOpts
defaultReportOpts = ReportOpts { goals = []
                               , showAllNuts = False
                               , showTags = []
                               , showAllTags = False
                               , oneColumn = False }


-- | NutRatio is not within the Food datatype, but reports use it. For
-- now this seems to be the best module to put this in.
newtype NutRatio = NutRatio { unNutRatio :: NonNeg }
                   deriving (Show, Exact, Rounded, Serialize)

nutRatio :: NutAmt -> NutAmt -> Maybe NutRatio
nutRatio (NutAmt x) (NutAmt y) = do
  q <- divide x y
  return $ NutRatio q

