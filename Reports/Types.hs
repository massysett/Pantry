module Reports.Types (GoalNameAmt(GoalNameAmt),
                      Report(Report, header, body, footer),
                      emptyRpt,
                      ReportOpts(ReportOpts, goals, showAllNuts,
                                 showTags, showAllTags,
                                 oneColumn),
                      defaultReportOpts) where

import Prelude(Bool(False))
import Data.Text(Text)
import qualified Data.Text as X (empty)
import Food(Food, Name, NutNamesAmts, NutAmt)

data GoalNameAmt = GoalNameAmt Name NutAmt

data Report =
  Report { header :: ReportOpts -> NutNamesAmts -> [Food] -> Text
         , body :: ReportOpts -> NutNamesAmts -> Food -> Text
         , footer :: ReportOpts -> NutNamesAmts -> [Food] -> Text }

emptyRpt :: Report
emptyRpt = Report { header = \_ _ _ -> X.empty
                  , body = \_ _ _ -> X.empty
                  , footer = \_ _ _ -> X.empty }

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


