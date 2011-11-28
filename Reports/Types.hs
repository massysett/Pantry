module Reports.Types (GoalNameAmt(GoalNameAmt),
                      Report(Report, header, body, footer),
                      emptyRpt,
                      ReportOpts(ReportOpts, goals, showAllNuts,
                                 showTags, showAllTags,
                                 oneColumn, totals),
                      defaultReportOpts) where

import Prelude(Bool(False))
import Data.Text(Text)
import qualified Data.Text as X (empty)
import Food(Food, Name, NutNamesAmts(NutNamesAmts), NutAmt)
import qualified Data.Map as M (empty)

data GoalNameAmt = GoalNameAmt Name NutAmt

data Report = Report { header :: ReportOpts -> [Food] -> Text
                     , body :: ReportOpts -> [Food] -> Food -> Text
                     , footer :: ReportOpts -> [Food] -> Text }

emptyRpt :: Report
emptyRpt = Report { header = \_ _ -> X.empty
                  , body = \_ _ _ -> X.empty
                  , footer = \_ _ -> X.empty }

data ReportOpts = ReportOpts { goals :: [GoalNameAmt]
                             , showAllNuts :: Bool
                             , showTags :: [Name]
                             , showAllTags :: Bool
                             , oneColumn :: Bool
                             , totals :: NutNamesAmts }

defaultReportOpts :: ReportOpts
defaultReportOpts = ReportOpts { goals = []
                               , showAllNuts = False
                               , showTags = []
                               , showAllTags = False
                               , oneColumn = False
                               , totals = NutNamesAmts M.empty }

