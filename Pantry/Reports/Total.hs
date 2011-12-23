module Pantry.Reports.Total (total) where

import qualified Pantry.Food as F
import Pantry.Reports.Types ( GoalNameAmt ( GoalNameAmt ),
                              showAllNuts,
                              goals,
                              ReportOpts )
import Pantry.Reports.Columns ( fmtColumnRow, txtColWidth, numColWidth )
import Pantry.Reports.ElemBy ( elemBy )
import Data.Text ( Text, pack, append )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as X
import Pantry.Exact(exact)
import Pantry.Rounded(rounded)
import qualified Pantry.Types as T

data TotGoalNut = TotGoalNut { totGoalName :: F.NutName
                             , totGoalGoal :: F.NutAmt
                             , totGoalTot :: Maybe F.NutAmt }

-- Name, Goal, Tot, %G
totRptColWidths :: [Int]
totRptColWidths = txtColWidth : replicate 2 numColWidth

totRptHdr :: X.Text
totRptHdr = X.concat [first, second, third] where
  first = pack "Total of all nutrients:\n"
  second = fmtColumnRow totRptColWidths
           . map pack $ ["Name", "Goal", "Tot", "%G"]
  third = (pack . concat . replicate n $ "-") `append` (pack "\n")
  n = sum totRptColWidths + numColWidth

showTotGoalNut :: TotGoalNut -> Text
showTotGoalNut n = fmtColumnRow totRptColWidths ts where
  ts = [na, goal, tot, pctG]
  na = exact . totGoalName $ n
  goal = rounded . totGoalGoal $ n
  tot = maybe X.empty rounded (totGoalTot n)
  pctG = fromMaybe X.empty $ do
    t <- totGoalTot n
    r <- t `T.divide` (totGoalGoal n)
    return
      . rounded
      . T.mult (F.NutAmt (T.partialNewNonNeg 100))
      $ r

data TotNonGoalNut = TotNonGoalNut { totNonName :: F.NutName
                                   , totNonAmt :: F.NutAmt }

showTotNonGoalNut :: TotNonGoalNut -> Text
showTotNonGoalNut n = fmtColumnRow totRptColWidths ts where
  ts = [na, goal, tot, pctG]
  na = exact . totNonName $ n
  goal = X.empty
  tot = rounded . totNonAmt $ n
  pctG = X.empty

getTotGoalNut :: M.Map F.NutName F.NutAmt -> GoalNameAmt -> TotGoalNut
getTotGoalNut m (GoalNameAmt n g) = TotGoalNut n g a where
  a = M.lookup n m

getTotNonGoalNuts :: M.Map F.NutName F.NutAmt -> [TotNonGoalNut]
getTotNonGoalNuts m = 
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

total :: ReportOpts -> M.Map F.NutName F.NutAmt -> X.Text
total o ts = hdr `append` txt `append` nl where
  txt
    | null . goals $ o = nonGoalTxt
    | otherwise = case showAllNuts o of
      True -> goalTxt `append` nonGoalTxt
      False -> goalTxt
  tgns = map (getTotGoalNut ts) (goals o)
  tans = getTotNonGoalNuts ts
  nonDupes = removeDupeTotNuts tgns tans
  goalTxt = X.concat . map showTotGoalNut $ tgns
  nonGoalTxt = X.concat . map showTotNonGoalNut $ nonDupes
  hdr = totRptHdr
  nl = pack "\n"


