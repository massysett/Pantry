module Reports where

import Food
import Types
import qualified Prelude as P
import Prelude hiding (lookup)
import qualified Data.List as L
import Data.List hiding (lookup)
import Data.List.Split
import qualified Data.Map as M
import Data.Map hiding (map, (\\), null)
import Data.Maybe
import Data.Either
import Data.Text hiding (map, null, drop, length, unlines, transpose,
                         zip, replicate, foldr)
import qualified Data.Text as X
import Data.Ratio
import Data.Decimal
import Reports.Columns
import Reports.Types
import Reports.Render

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
    name = render o . totGoalName $ n
    goal = render o . totGoalGoal $ n
    tot = maybe X.empty (render o) (totGoalTot n)
    pctG = maybe X.empty id $ do
      let g = totGoalGoal n
      t <- totGoalTot n
      r <- nutRatio t g
      return . render o $ r

data TotNonGoalNut = TotNonGoalNut { totNonName :: Name
                                   , totNonAmt :: NutAmt }

instance Render TotNonGoalNut where
  render o n = fmtColumnRow totRptColWidths ts where
    ts = [name, goal, tot, pctG]
    name = render o . totNonName $ n
    goal = X.empty
    tot = render o . totNonAmt $ n
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

totRptTxt :: NutNamesAmts -> ReportOpts -> X.Text
totRptTxt ts o = hdr `append` txt `append` nl where
  txt
    | null . goals $ o = nonGoalTxt
    | otherwise = case showAllNuts o of
      True -> goalTxt `append` nonGoalTxt
      False -> goalTxt
  tgns = map (getTotGoalNut ts) (goals o)
  tngns = getTotNonGoalNuts ts
  nonDupes = removeDupeTotNuts tgns tngns
  goalTxt = X.concat . map (render o) $ tgns
  nonGoalTxt = X.concat . map (render o) $ tngns
  hdr = totRptHdr
  nl = pack "\n"

totRpt :: Report
totRpt = emptyRpt { footer = g } where
  g o fs = totRptTxt ts o where
    ts = foldFoodNuts fs

label :: (Render a) => String -> ReportOpts -> a -> X.Text
label l o d = pack l `append` pack ": " `append` render o d

-- Multi column property report:
-- 2 1/2 cups (83 g)
-- %R: .14    Yield: 240 g
-- ID: 9898

-- Single column property report:
-- Quantity: 2 1/2
-- Unit name: cups
-- Unit amount: 34 g
-- Total weight: 85 g
-- Percent refuse: .14
-- Yield: 240 g
-- ID: 9898

data QtyUnitProp = QtyUnitProp Food

{-
instance Render QtyUnitProp where
  render o (QtyUnitProp f) = txt where
    txt = if oneColumn o then oneCol else twoCol
    oneCol = X.unlines [qty, unn, una, wei, pr, yld, id] where
-}    
