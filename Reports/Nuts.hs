module Reports.Nuts (nuts) where

import Data.Text hiding (map, foldr, null, replicate)
import qualified Data.Text as X
import Food
import Reports.Render
import Reports.Types
import Data.Map hiding (map, null)
import qualified Data.Map as M
import Reports.Columns
import qualified Data.List as L
import Reports.ElemBy

data GoalNut = GoalNut { goalNutName :: Name
                       , goalNutGoal :: NutAmt
                       , goalNutAmt :: Maybe NutAmt
                       , goalTotalAmt :: Maybe NutAmt }

instance Render GoalNut where
  render o n = fmtColumnRow nutRptColWidths ts where
    ts = [name, nutAmt, pctGoal, pctTot]
    name = render o . goalNutName $ n
    nutAmt = maybe X.empty (render o) (goalNutAmt n)
    pctGoal = maybe X.empty id $ do
      let g = goalNutGoal n
      a <- goalNutAmt n
      r <- nutRatio a g
      return . render o $ r
    pctTot = maybe X.empty id $ do
      t <- goalTotalAmt n
      a <- goalNutAmt n
      r <- nutRatio a t
      return . render o $ r

nutRptColWidths :: [Int]
nutRptColWidths = [txtColWidth, numColWidth, numColWidth]

nutRptHdr :: X.Text
nutRptHdr = X.concat [first, second] where
  first = fmtColumnRow nutRptColWidths
          . map pack $ ["Name", "Amt", "%G", "%T"]
  second = (pack . L.concat . replicate n $ "-") `append` (pack "\n")
  n = sum nutRptColWidths + numColWidth

data NonGoalNut = NonGoalNut { nonGoalNutName :: Name 
                             , nonGoalNutAmt :: NutAmt
                             , nonGoalTotalAmt :: Maybe NutAmt }
instance Render NonGoalNut where
  render o n = fmtColumnRow nutRptColWidths ts where
    ts = [name, nutAmt, pctGoal, pctTot]
    name = render o . nonGoalNutName $ n
    nutAmt = render o . nonGoalNutAmt $ n
    pctGoal = X.empty
    pctTot = maybe X.empty id $ do
      t <- nonGoalTotalAmt n
      let a = nonGoalNutAmt n
      r <- nutRatio a t
      return . render o $ r

getGoalNut :: NutNamesAmts -> Food -> GoalNameAmt -> GoalNut
getGoalNut t f (GoalNameAmt n ng) = GoalNut n ng gna gta where
  gna = getNut n f
  gta = M.lookup n . (\(NutNamesAmts m) -> m) $ t

nonGoalNuts :: NutNamesAmts -> Food -> [NonGoalNut]
nonGoalNuts ts f = map toNonGoalNut $ M.assocs m where
  (NutNamesAmts m) = foodNuts f
  toNonGoalNut (n, a) = NonGoalNut n a (M.lookup n m)

appendNonGoalIfNotDupe ::
  [GoalNut]
     -> NonGoalNut
     -> [NonGoalNut]
     -> [NonGoalNut]
appendNonGoalIfNotDupe gns ngn ngns
  | isDupe = ngns
  | otherwise = ngn : ngns where
    isDupe = elemBy f gns
    f gn = goalNutName gn == nonGoalNutName ngn

removeDupeNonGoalNuts :: [GoalNut] -> [NonGoalNut] -> [NonGoalNut]
removeDupeNonGoalNuts gns = foldr (appendNonGoalIfNotDupe gns) []

nutRptTxt :: NutNamesAmts -- ^ Totals
             -> ReportOpts
             -> Food
             -> X.Text
nutRptTxt ts o f = nutRptHdr `append` txt `append` gap where
  txt
    | null . goals $ o = nonGoalTxt
    | otherwise = case showAllNuts o of
      True -> goalTxt `append` nonGoalTxt
      False -> goalTxt
  ngns = nonGoalNuts ts f
  gns = map (getGoalNut ts f) (goals o)
  nonDupes = removeDupeNonGoalNuts gns ngns
  goalTxt = X.concat . map (render o) $ gns
  nonGoalTxt = X.concat . map (render o) $ ngns
  gap = pack "\n"

nuts :: Report
nuts = emptyRpt { body = f } where
  f o fs food = X.concat . map (nutRptTxt ts o) $ fs where
    ts = foldFoodNuts fs

