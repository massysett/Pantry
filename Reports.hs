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
                         zip, replicate)
import qualified Data.Text as X
import Data.Ratio
import Data.Decimal

class Render a where
  render :: ReportOpts -> a -> Text

instance Render NonNeg where
  render _ = pack . show . round . nonNegToRational

instance Render NonNegMixed where
  render _ n = result where
    result = case (d, r) of
      (Just ds, Just rs) -> ds `snoc` ' ' `append` rs
      (Just ds, Nothing) -> ds
      (Nothing, Just rs) -> rs
      (Nothing, Nothing) -> pack "0"
    nd = mixedDec n
    nr = mixedRatio n
    d | nd == Decimal 0 0 = Nothing
      | otherwise = Just . pack . show $ nd
    r | nr == (0 % 1) = Nothing
      | otherwise = Just . pack $ num ++ "/" ++ den where
        num = show . numerator $ nr
        den = show . denominator $ nr

instance Render BoundedPercent where
  render o = render o . pctToMixed

instance Render Name where
  render _ (Name t) = t

instance Render NutAmt where render o (NutAmt n) = render o n

nutWidth :: Int
nutWidth = 35

instance Render NutNameAmt where
  render o (NutNameAmt n a) = label `append` amt where
    label = rpad nutWidth . render o $ n
    amt = render o a

instance Render Grams where render o (Grams n) = render o n
instance Render MixedGrams where render o (MixedGrams n) = render o n

instance Render UnitNameAmt where
  render o (UnitNameAmt n g) = txt where
    txt = nt `append` open `append` gt `append` close
    nt = render o n
    open = pack " ("
    gt = render o g
    close = pack " g)"

instance Render UnitNamesAmts where
  render o (UnitNamesAmts m) = r where
    r = X.concat . map toLine . map toNameAmt . M.assocs $ m
    toLine u = blank `append` (render o u) `snoc` '\n' where
      blank = pack . replicate 4 $ ' '
    toNameAmt = uncurry UnitNameAmt

instance Render TagVal where
  render _ (TagVal t) = t

instance Render TagNameVal where
  render o (TagNameVal n v) = r where
    r = lbl `append` col `append` val `snoc` '\n'
    lbl = render o n
    col = pack ": "
    val = render o v

instance Render PctRefuse where
  render o (PctRefuse b) = l `append` v where
    l | oneColumn o = pack "Percent refuse: "
      | otherwise = pack "%R: "
    v = render o b

instance Render TagNamesVals where
  render o (TagNamesVals m) = X.unlines . makeCols . makeLines $ m where
    makeCols | oneColumn o = id
             | otherwise = map X.concat . columns 2
    makeLines = map (render o)
                . map (uncurry TagNameVal)
                . M.assocs

data Report = Report { header :: ReportOpts -> [Food] -> Text
                     , body :: ReportOpts -> [Food] -> Food -> Text
                     , footer :: ReportOpts -> [Food] -> Text }

emptyRpt :: Report
emptyRpt = Report { header = \_ _ -> X.empty
                  , body = \_ _ _ -> X.empty
                  , footer = \_ _ -> X.empty }

data ReportOpts = ReportOpts { goals :: [NutNameAmt]
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

name :: Report
name = emptyRpt {body = b} where
  b _ _ f = snoc n '\n' where
    n = case getTag t f of
      Nothing -> pack "(No name)"
      (Just (TagNameVal _ (TagVal v))) -> v
    t = Name . pack $ "name"

tagRpt :: Report
tagRpt = emptyRpt {body = b} where
  b o _ f = listToCols cs ss where
    cs = if oneColumn o then [] else [35]
    ss = map toString ts
    toString ((Name n), (TagVal v)) =
      n `append` (pack ": ") `append` v
    ts = orderedNV ++ restNV
    (TagNamesVals m) = tags f
    orderedNV = catMaybes . map (lookupPair m) . showTags $ o
    unorderedNV = assocs m \\ orderedNV
    restNV | null (showTags o) || showAllTags o = unorderedNV
           | otherwise = []
    
lookupPair ::(Ord k) => Map k v -> k -> Maybe (k, v)
lookupPair m k = do
  v <- lookup k m
  return (k, v)
    

columns :: Int -> [a] -> [[a]]
columns n ls = transpose . splitEvery s $ ls where
  s = if r == 0 then q else q + 1
  (q, r) = length ls `divMod` n

rpad :: Int -> Text -> Text
rpad l = justifyLeft l ' '

colsToString :: [Int] -> [Text] -> Text
colsToString is ss = firsts `append` lasts `snoc` '\n' where
  firsts = X.concat . map (uncurry rpad) . zip is $ ss
  lasts = X.concat . drop (length is) $ ss

colsListToString :: [Int] -> [[Text]] -> Text
colsListToString is tss = X.concat . map (colsToString is) $ tss

listToCols :: [Int] -> [Text] -> Text
listToCols ls = colsListToString ls . columns (length ls + 1)

blank :: Report
blank = emptyRpt {body = b} where
  b _ _ _ = pack "\n"

unitsRpt :: Report
unitsRpt = emptyRpt {body = b} where
  b _ _ f = X.concat . map toString $ (assocs m) where
    toString ((Name k), _) =
      (pack . replicate 3 $ ' ') `append` k `snoc` '\n'
    (UnitNamesAmts m) = allAvailUnits f

-- Nut rpt
data GoalNameAmt = GoalNameAmt Name NutAmt
data ActualFoodAmt = ActualFoodAmt (Maybe NutAmt)
data ActualTotalAmt = ActualTotalAmt 
data GoalNut = GoalNut GoalNameAmt (Maybe NutAmt) 

{-
propertiesRpt :: Report
propertiesRpt = emptyRpt {body = b} where
  b o _ f = if oneColumn o then long else brief where
    long = X.unlines [q, un, ua, r, y, i] where
      q = "Quantity: " ++ show (qty f)
      un = "Unit name: " ++ show unitName where
        (UnitNameAmt unitName _) = currUnit f
      ua = "Unit amount: " ++ amt ++ " grams" where
        amt = show . round $ amtGr
        (UnitNameAmt _ (Grams amtGr)) = currUnit f
      r = "Percent refuse: " ++ show (round (pctRefuse f))
      y = "Yield: " ++ yldStr where
        yldStr = case yield f of
          Nothing -> "(none)"
          (Just m) -> show m ++ " grams"
      i = "ID: " ++ show (foodId f)
    brief = undefined
-}
