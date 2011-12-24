module Pantry.Reports.Types (GoalNameAmt(GoalNameAmt),
                      ReportOpts(ReportOpts, goals, showAllNuts,
                                 showTags, showAllTags,
                                 oneColumn),
                      FoodRpt, TotalRpt,
                      defaultReportOpts) where

import Data.Text(Text)
import Pantry.Food(Food, NutName, NutAmt, TagName)
import Pantry.Tray(Tray)
import qualified Data.Map as M

data GoalNameAmt = GoalNameAmt NutName NutAmt

type FoodRpt = ReportOpts -> M.Map NutName NutAmt -> Food -> Text
type TotalRpt = ReportOpts -> M.Map NutName NutAmt -> Tray -> [Food] -> Text

data ReportOpts = ReportOpts { goals :: [GoalNameAmt]
                             , showAllNuts :: Bool
                             , showTags :: [TagName]
                             , showAllTags :: Bool
                             , oneColumn :: Bool }

defaultReportOpts :: ReportOpts
defaultReportOpts = ReportOpts { goals = []
                               , showAllNuts = False
                               , showTags = []
                               , showAllTags = False
                               , oneColumn = False }
