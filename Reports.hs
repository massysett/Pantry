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
import Data.Text hiding (map, null, drop, length, unlines, transpose,
                         zip, replicate)
import qualified Data.Text as X
import Data.Ratio
import Data.Decimal

class Render a where
  render :: a -> Text

instance Render NonNeg where
  render = pack . show . round . nonNegToRational

instance Render NonNegMixed where
  render n = result where
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
  render = render . pctToMixed

instance Render Name where
  render (Name t) = t

instance Render NutAmt where render (NutAmt n) = render n

instance Render Grams where render (Grams n) = render n
instance Render MixedGrams where render (MixedGrams n) = render n

instance Render UnitNameAmt where
  render (UnitNameAmt n g) = txt where
    txt = nt `append` open `append` gt `append` close
    nt = render n
    open = pack " ("
    gt = render g
    close = pack " g)"

instance Render UnitNamesAmts where
  render (UnitNamesAmts m) = r where
    r = X.concat . map toLine . map toNameAmt . M.assocs $ m
    toLine u = blank `append` render u `snoc` '\n' where
      blank = pack . replicate 4 $ ' '
    toNameAmt = uncurry UnitNameAmt

instance Render TagVal where
  render (TagVal t) = t

instance Render TagNameVal where
  render (TagNameVal n v) = r where
    r = lbl `append` col `append` val `snoc` '\n'
    lbl = render n
    col = pack ": "
    val = render v

newtype PctRefuseShort = PctRefuseShort PctRefuse
instance Render PctRefuseShort where
  render (PctRefuseShort (PctRefuse b)) = l `append` v where
    l = pack "%R: "
    v = render b

newtype PctRefuseLong = PctRefuseLong PctRefuse
instance Render PctRefuseLong where
  render (PctRefuseLong (PctRefuse b)) = l `append` v where
    l = pack "Percent refuse: "
    v = render b

newtype TagNamesValsOneCol = TagNamesValsOneCol TagNamesVals

instance Render TagNamesValsOneCol where
  render (TagNamesValsOneCol (TagNamesVals m)) = r where
    r = X.unlines
        . map render
        . map (uncurry TagNameVal)
        . M.assocs $ m

newtype TagNamesValsTwoCols = TagNamesValsTwoCols TagNamesVals

instance Render TagNamesValsTwoCols where
  render (TagNamesValsTwoCols (TagNamesVals m)) = r where
    r = X.unlines
        . map X.concat
        . columns 2
        . map render
        . map (uncurry TagNameVal)
        . M.assocs $ m

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
