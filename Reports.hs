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
import Data.Text hiding (map, null, drop, length, unlines, transpose,
                         zip, replicate)
import qualified Data.Text as X


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
                             , oneColumn :: Bool }

defaultReportOpts :: ReportOpts
defaultReportOpts = ReportOpts { goals = []
                               , showAllNuts = False
                               , showTags = []
                               , showAllTags = False
                               , oneColumn = False }

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
