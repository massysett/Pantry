module Pantry.Reports.Total (total) where

import Pantry.Food
import Pantry.Reports.Types
import Pantry.Reports.Render
import Pantry.Reports.Columns
import Pantry.Reports.ElemBy
import Data.Text hiding (null, replicate, map, foldr)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as X
import Pantry.Exact(exact)
import Pantry.Rounded(rounded)

data TotGoalNut = TotGoalNut { totGoalName :: Name
                             , totGoalGoal :: NutAmt
                             , totGoalTot :: Maybe NutAmt }

-- Name, Goal, Tot, %G
totRptColWidths :: [Int]
totRptColWidths = txtColWidth : replicate 2 numColWidth

totRptHdr :: X.Text
totRptHdr = X.concat [first, second, third] where
  first = pack "Total of all nutrients:\n"
  second = fmtColumnRow totRptColWidths
           . map pack $ ["Name", "Goal", "Tot", "%G"]
  third = (pack . L.concat . replicate n $ "-") `append` (pack "\n")
  n = sum totRptColWidths + numColWidth

instance Render TotGoalNut where
  render o n = fmtColumnRow totRptColWidths ts where
    ts = [name, goal, tot, pctG]
    name = exact . totGoalName $ n
    goal = rounded . totGoalGoal $ n
    tot = maybe X.empty rounded (totGoalTot n)
    pctG = maybe X.empty id $ do
      let g = totGoalGoal n
      t <- totGoalTot n
      r <- nutRatio t g
      return . render o $ r

data TotNonGoalNut = TotNonGoalNut { totNonName :: Name
                                   , totNonAmt :: NutAmt }

instance Render TotNonGoalNut where
  render _ n = fmtColumnRow totRptColWidths ts where
    ts = [name, goal, tot, pctG]
    name = exact . totNonName $ n
    goal = X.empty
    tot = rounded . totNonAmt $ n
    pctG = X.empty

getTotGoalNut :: NutNamesAmts -> GoalNameAmt -> TotGoalNut
getTotGoalNut nna (GoalNameAmt n g) = TotGoalNut n g a where
  (NutNamesAmts m) = nna
  a = M.lookup n m

getTotNonGoalNuts :: NutNamesAmts -> [TotNonGoalNut]
getTotNonGoalNuts (NutNamesAmts m) =
  map (uncurry TotNonGoalNut) $ M.assocs m

appendTotIfNotDupe :: [TotGoalNut]
                      -> TotNonGoalNut
                      -> [TotNonGoalNut]
                      -> [TotNonGoalNut]
appendTotIfNotDupe gns ng ngs
  | isDupe = ngs
  | otherwise = ng : ngs where
    isDupe = elemBy f gns
    f gn = totGoalName gn == totNonName ng

removeDupeTotNuts :: [TotGoalNut] -> [TotNonGoalNut] -> [TotNonGoalNut]
removeDupeTotNuts gns = foldr (appendTotIfNotDupe gns) []

total :: ReportOpts -> NutNamesAmts -> X.Text
total o ts = hdr `append` txt `append` nl where
  txt
    | null . goals $ o = nonGoalTxt
    | otherwise = case showAllNuts o of
      True -> goalTxt `append` nonGoalTxt
      False -> goalTxt
  tgns = map (getTotGoalNut ts) (goals o)
  tans = getTotNonGoalNuts ts
  nonDupes = removeDupeTotNuts tgns tans
  goalTxt = X.concat . map (render o) $ tgns
  nonGoalTxt = X.concat . map (render o) $ nonDupes
  hdr = totRptHdr
  nl = pack "\n"


