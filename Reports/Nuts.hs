module Reports.Nuts (nuts) where

import Data.Text hiding (map, foldr, null, replicate)
import qualified Data.Text as X
import Food
import Reports.Render
import Reports.Types
import qualified Data.Map as M
import Reports.Columns
import qualified Data.List as L
import Reports.ElemBy
import Rounded(rounded)
import Exact(exact)

data GoalNut = GoalNut { goalNutName :: Name
                       , goalNutGoal :: NutAmt
                       , goalNutAmt :: Maybe NutAmt
                       , goalTotalAmt :: Maybe NutAmt }

instance Render GoalNut where
  render o n = fmtColumnRow nutRptColWidths ts where
    ts = [name, nutAmt, pctGoal, pctTot]
    name = exact . goalNutName $ n
    nutAmt = maybe X.empty rounded (goalNutAmt n)
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

data AnyNut = AnyNut { nonGoalNutName :: Name 
                             , nonGoalNutAmt :: NutAmt
                             , nonGoalTotalAmt :: Maybe NutAmt }
instance Render AnyNut where
  render o n = fmtColumnRow nutRptColWidths ts where
    ts = [name, nutAmt, pctGoal, pctTot]
    name = exact . nonGoalNutName $ n
    nutAmt = rounded . nonGoalNutAmt $ n
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

-- | Returns all nutrients from a food, whether they have a goal or not.
anyNuts :: NutNamesAmts -- ^ Totals
           -> Food
           -> [AnyNut]
anyNuts (NutNamesAmts ts) f = map toAnyNut . M.assocs $ m where
  (NutNamesAmts m) = foodNuts f
  toAnyNut (n, a) = AnyNut n a (M.lookup n ts)

appendAnyIfNotDupe ::
  [GoalNut]
     -> AnyNut
     -> [AnyNut]
     -> [AnyNut]
appendAnyIfNotDupe gns ngn ngns
  | isDupe = ngns
  | otherwise = ngn : ngns where
    isDupe = elemBy f gns
    f gn = goalNutName gn == nonGoalNutName ngn

removeDupeAnyNuts :: [GoalNut] -> [AnyNut] -> [AnyNut]
removeDupeAnyNuts gns = foldr (appendAnyIfNotDupe gns) []

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
  ngns = anyNuts ts f
  gns = map (getGoalNut ts f) (goals o)
  nonDupes = removeDupeAnyNuts gns ngns
  goalTxt = X.concat . map (render o) $ gns
  nonGoalTxt = X.concat . map (render o) $ nonDupes
  gap = pack "\n"

nuts :: Report
nuts = emptyRpt { body = f } where
  f o fs food = nutRptTxt ts o food where
    ts = foldFoodNuts fs

