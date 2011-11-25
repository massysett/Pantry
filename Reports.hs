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

nutWidth :: Int
nutWidth = 35

-- Nut rpt
data GoalNut = GoalNut { goalNutName :: Name
                       , goalNutGoal :: NutAmt
                       , goalNutAmt :: Maybe NutAmt
                       , goalTotalAmt :: Maybe NutAmt }

nutRptColWidths :: [Int]
nutRptColWidths = [txtColWidth, numColWidth, numColWidth]

nutRptHdr :: X.Text
nutRptHdr = X.concat [first, second] where
  first = fmtColumnRow nutRptColWidths
          . map pack $ ["Name", "Amt", "%G", "%T"]
  second = (pack . L.concat . replicate n $ "-") `append` (pack "\n")
  n = sum nutRptColWidths + numColWidth

instance Render NutRatio where
  render _ (NutRatio nn) =
    pack . show . round . (* 100) . nonNegToRational $ nn

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

elemBy :: (a -> Bool) -> [a] -> Bool
elemBy f = foldr g False where
  g _ True = True
  g a False = f a

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

nutRpt :: Report
nutRpt = emptyRpt { body = f } where
  f o fs food = X.concat . map (nutRptTxt ts o) $ fs where
    ts = foldFoodNuts fs

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
