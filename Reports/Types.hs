module Reports.Types (GoalNameAmt(GoalNameAmt),
                      ReportOpts(ReportOpts, goals, showAllNuts,
                                 showTags, showAllTags,
                                 oneColumn),
                      FoodRpt, TotalRpt,
                      defaultReportOpts) where

import Prelude(Bool(False))
import Data.Text(Text)
import qualified Data.Text as X (empty)
import Food(Food, Name, NutNamesAmts, NutAmt)
import Control.Applicative(Applicative)
import Db(Db)

data GoalNameAmt = GoalNameAmt Name NutAmt

type FoodRpt = ReportOpts -> NutNamesAmts -> Food -> Text
type TotalRpt f = ReportOpts -> NutNamesAmts -> Db -> f Food -> Text

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


