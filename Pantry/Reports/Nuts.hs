module Pantry.Reports.Nuts (nuts) where

import Prelude((.), ($), maybe, return, Int, (==), map,
               Maybe, id, replicate, sum, (+), otherwise,
               foldr, null, Bool(True, False))
import Data.Text (empty, pack, append, Text, concat)
import qualified Pantry.Food as F
import Pantry.Reports.Types(GoalNameAmt(GoalNameAmt), ReportOpts,
                     goals, showAllNuts)
import Data.Map (lookup, assocs)
import qualified Data.Map as M
import Pantry.Reports.Columns(fmtColumnRow, txtColWidth, numColWidth)
import qualified Data.List as L (concat)
import Pantry.Reports.ElemBy(elemBy)
import Pantry.Rounded(rounded)
import Pantry.Exact(exact)
import Data.Maybe ( fromMaybe )
import qualified Pantry.Types as T

data GoalNut = GoalNut { goalNutName :: F.NutName
                       , goalNutGoal :: F.NutAmt
                       , goalNutAmt :: Maybe F.NutAmt
                       , goalTotalAmt :: Maybe F.NutAmt }

showGoalNut :: GoalNut -> Text
showGoalNut n = fmtColumnRow nutRptColWidths ts where
  ts = [name, nutAmt, pctGoal, pctTot]
  name = exact . goalNutName $ n
  nutAmt = maybe empty rounded (goalNutAmt n)
  pctGoal = fromMaybe empty $ do
    let g = goalNutGoal n
    a <- goalNutAmt n
    r <- a `T.divide` g
    return
      . rounded
      . T.mult (F.NutAmt . T.partialNewNonNeg $ 100)
      $ r
  pctTot = fromMaybe empty $ do
    t <- goalTotalAmt n
    a <- goalNutAmt n
    r <- a `T.divide` t
    return
      . rounded
      . T.mult (F.NutAmt . T.partialNewNonNeg $ 100)
      $ r

nutRptColWidths :: [Int]
nutRptColWidths = [txtColWidth, numColWidth, numColWidth]

nutRptHdr :: Text
nutRptHdr = concat [first, second] where
  first = fmtColumnRow nutRptColWidths
          . map pack $ ["Name", "Amt", "%G", "%T"]
  second = (pack . L.concat . replicate n $ "-") `append` (pack "\n")
  n = sum nutRptColWidths + numColWidth

data AnyNut = AnyNut { nonGoalNutName :: F.NutName 
                     , nonGoalNutAmt :: F.NutAmt
                     , nonGoalTotalAmt :: Maybe F.NutAmt }

showAnyNut :: AnyNut -> Text
showAnyNut n = fmtColumnRow nutRptColWidths ts where
  ts = [name, nutAmt, pctGoal, pctTot]
  name = exact . nonGoalNutName $ n
  nutAmt = rounded . nonGoalNutAmt $ n
  pctGoal = empty
  pctTot = fromMaybe empty $ do
    t <- nonGoalTotalAmt n
    let a = nonGoalNutAmt n
    r <- a `T.divide` t
    return
      . rounded
      . T.mult (F.NutAmt . T.partialNewNonNeg $ 100)
      $ r

getGoalNut :: M.Map F.NutName F.NutAmt -> F.Food -> GoalNameAmt -> GoalNut
getGoalNut t f (GoalNameAmt n ng) = GoalNut n ng gna gta where
  gna = lookup n . F.getNuts $ f
  gta = lookup n t

-- | Returns all nutrients from a food, whether they have a goal or not.
anyNuts :: M.Map F.NutName F.NutAmt -- ^ Totals
           -> F.Food
           -> [AnyNut]
anyNuts ts f = map toAnyNut . assocs $ m where
  m = F.getNuts f
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
             -> M.Map F.NutName F.NutAmt
             -> F.Food
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
  goalTxt = concat . map showGoalNut $ gns
  nonGoalTxt = concat . map showAnyNut $ nonDupes
  gap = pack "\n"
