module Pantry.Reports.Types (GoalNameAmt(GoalNameAmt),
                      ReportOpts(ReportOpts, goals, showAllNuts,
                                 showTags, showAllTags,
                                 oneColumn),
                      FoodRpt, TotalRpt,
                      defaultReportOpts) where

import Data.Text(Text)
import Pantry.Food(Food, NutName, NutAmt(NutAmt))
import Pantry.Tray(Tray)
import qualified Data.Map as M
import qualified Pantry.Types as T

data GoalNameAmt = GoalNameAmt NutName NutAmt

type FoodRpt = ReportOpts -> M.Map NutName NutAmt -> Food -> Text
type TotalRpt = ReportOpts -> M.Map NutName NutAmt -> Tray -> [Food] -> Text

data ReportOpts = ReportOpts { goals :: [GoalNameAmt]
                             , showAllNuts :: Bool
                             , showTags :: [NutName]
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
newtype NutRatio = NutRatio { unNutRatio :: T.NonNeg } deriving Show

nutRatio :: NutAmt -> NutAmt -> Maybe NutRatio
nutRatio (NutAmt x) (NutAmt y) = do
  q <- T.divide x y
  return $ NutRatio q

