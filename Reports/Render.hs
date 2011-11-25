-- | Data types that have a Render instance that is universal across
-- report types can be defined here. Other Render instances should be
-- declared in the module in which they are used.
module Reports.Render where

import Reports.Types
import Types
import Food
import Data.Text hiding (map, replicate)
import qualified Data.Map as M
import Data.Ratio
import Data.Decimal
import qualified Data.Text as X
import Reports.Columns

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

instance Render NutNameAmt where
  render o (NutNameAmt n a) = label `append` amt where
    label = rpad txtColWidth . render o $ n
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
  render o (TagNamesVals m) = makeCols . makeLines $ m where
    makeCols | oneColumn o = X.concat
             | otherwise = newspaper [35]
    makeLines = map (render o)
                . map (uncurry TagNameVal)
                . M.assocs

