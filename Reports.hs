module Reports where

import Food
import qualified Prelude as P
import Prelude hiding (lookup)
import qualified Data.List as L
import Data.List hiding (lookup)
import Data.List.Split
import qualified Data.Map as M
import Data.Map hiding (map, (\\), null)
import Data.Maybe

data Report = Report { header :: ReportOpts -> [Food] -> String
                     , body :: ReportOpts -> [Food] -> Food -> String
                     , footer :: ReportOpts -> [Food] -> String }

emptyRpt :: Report
emptyRpt = Report { header = \_ _ -> ""
                  , body = \_ _ _ -> ""
                  , footer = \_ _ -> "" }

data ReportOpts = ReportOpts { goals :: [NutNameAmt]
                             , showAllNuts :: Bool
                             , showTags :: [Name]
                             , showAllTags :: Bool
                             , oneColumn :: Bool }

defaultReportOpts :: ReportOpts
defaultReportOpts = ReportOpts { goals = []
                               , showAllNuts = False
                               , showTags = []
                               , showAllTags = False
                               , oneColumn = False }

name :: Report
name = emptyRpt {body = b} where
  b _ _ f = n ++ "\n" where
    n = case getTag t f of
      Nothing -> "(No name)"
      (Just (TagNameVal _ (TagVal v))) -> show v
    t = Name "name"

tagRpt :: Report
tagRpt = emptyRpt {body = b} where
  b o _ f = listToCols cs ss where
    cs = if oneColumn o then [] else [35]
    ss = map toString ts
    toString (n, v) = show n ++ ": " ++ show v
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

rpad :: Int -> String -> String
rpad l s = s ++ p where
  p | length s >= l = " "
    | otherwise = replicate (l - length s) ' '

colsToString :: [Int] -> [String] -> String
colsToString is ss = firsts ++ lasts ++ "\n" where
  firsts = concatMap (uncurry rpad) . zip is $ ss
  lasts = concat . drop (length is) $ ss

colsListToString :: [Int] -> [[String]] -> String
colsListToString = concatMap . colsToString

listToCols :: [Int] -> [String] -> String
listToCols ls = colsListToString ls . columns (length ls + 1)

blank :: Report
blank = emptyRpt {body = b} where
  b _ _ _ = "\n"

unitsRpt :: Report
unitsRpt = emptyRpt {body = b} where
  b _ _ f = concatMap toString (assocs m) where
    toString (k, _) = replicate 3 ' ' ++ show k ++ "\n"
    (UnitNamesAmts m) = allAvailUnits f

propertiesRpt :: Report
propertiesRpt = emptyRpt {body = b} where
  b o _ f = if oneColumn o then long else brief where
    long = unlines [q, un, ua, r, y, i] where
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
