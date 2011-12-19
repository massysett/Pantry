module Pantry.Reports.Nuts (nuts) where

import Prelude((.), ($), maybe, return, Int, (==), map,
               Maybe, id, replicate, sum, (+), otherwise,
               foldr, null, Bool(True, False))
import Data.Text (empty, pack, append, Text, concat)
import Pantry.Food(NutAmt, nutRatio, Food, getNut,
            NutNamesAmts(NutNamesAmts), foodNuts,
            Name(Name))
import Pantry.Reports.Render(Render(render))
import Pantry.Reports.Types(GoalNameAmt(GoalNameAmt), ReportOpts,
                     goals, showAllNuts)
import Data.Map (lookup, assocs)
import Pantry.Reports.Columns(fmtColumnRow, txtColWidth, numColWidth)
import qualified Data.List as L (concat)
import Pantry.Reports.ElemBy(elemBy)
import Pantry.Rounded(rounded)
import Pantry.Exact(exact)

data GoalNut = GoalNut { goalNutName :: Name
                       , goalNutGoal :: NutAmt
                       , goalNutAmt :: Maybe NutAmt
                       , goalTotalAmt :: Maybe NutAmt }

instance Render GoalNut where
  render o n = fmtColumnRow nutRptColWidths ts where
    ts = [name, nutAmt, pctGoal, pctTot]
    name = exact . goalNutName $ n
    nutAmt = maybe empty rounded (goalNutAmt n)
    pctGoal = maybe empty id $ do
      let g = goalNutGoal n
      a <- goalNutAmt n
      r <- nutRatio a g
      return . render o $ r
    pctTot = maybe empty id $ do
      t <- goalTotalAmt n
      a <- goalNutAmt n
      r <- nutRatio a t
      return . render o $ r

nutRptColWidths :: [Int]
nutRptColWidths = [txtColWidth, numColWidth, numColWidth]

nutRptHdr :: Text
nutRptHdr = concat [first, second] where
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
    pctGoal = empty
    pctTot = maybe empty id $ do
      t <- nonGoalTotalAmt n
      let a = nonGoalNutAmt n
      r <- nutRatio a t
      return . render o $ r

getGoalNut :: NutNamesAmts -> Food -> GoalNameAmt -> GoalNut
getGoalNut t f (GoalNameAmt n ng) = GoalNut n ng gna gta where
  gna = getNut n f
  gta = lookup n . (\(NutNamesAmts m) -> m) $ t

-- | Returns all nutrients from a food, whether they have a goal or not.
anyNuts :: NutNamesAmts -- ^ Totals
           -> Food
           -> [AnyNut]
anyNuts (NutNamesAmts ts) f = map toAnyNut . assocs $ m where
  (NutNamesAmts m) = foodNuts f
  toAnyNut (n, a) = AnyNut n a (lookup n ts)

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

nuts :: ReportOpts
             -> NutNamesAmts
             -> Food
             -> Text
nuts o ts f = nutRptHdr `append` txt `append` gap where
  txt
    | null . goals $ o = nonGoalTxt
    | otherwise = case showAllNuts o of
      True -> goalTxt `append` nonGoalTxt
      False -> goalTxt
  ngns = anyNuts ts f
  gns = map (getGoalNut ts f) (goals o)
  nonDupes = removeDupeAnyNuts gns ngns
  goalTxt = concat . map (render o) $ gns
  nonGoalTxt = concat . map (render o) $ nonDupes
  gap = pack "\n"
