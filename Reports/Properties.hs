module Reports.Properties (properties) where

import Reports.Types (Report(..), emptyRpt,
                      ReportOpts(..))
import Reports.Render
import Data.Text
import qualified Data.Text as X
import Food
import Data.Ratio
import Types
import Data.Decimal

properties :: Report
properties = undefined

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
data QtyUnitAmt = QtyUnitAmt Food

label :: String -> Text -> Text
label s t = pack s `append` pack ": " `append` t `snoc` '\n'

-- | Render a number-like datatype exactly -- without rounding.
class Exact a where
  exact :: a -> Text

instance (Integral a) => Exact (Ratio a) where
  exact r = num `snoc` '/' `append` denom where
    num = pack . show . numerator $ r
    denom = pack . show . denominator $ r

instance (Integral i) => Exact (DecimalRaw i) where
  exact = pack . show

instance Exact Qty where
  exact (Qty q) = case q of
    (Left nn) -> exact . nonNegToRational $ nn
    (Right nnm) ->
      let d = exact . mixedDec $ nnm
          r = exact . mixedRatio $ nnm
      in d `snoc` ' ' `append` r

instance Exact Grams where
  exact (Grams nn) = exact . nonNegToRational $ nn

instance Render QtyUnitAmt where
  render o (QtyUnitAmt f) = case oneColumn o of
    True -> let q = label "Quantity" (exact . qty $ f)
                (UnitNameAmt (Name unit) amt) = currUnit f
                u = label "Unit name" unit
                a = label "Unit amount" (exact amt)
                g = label "Total weight" (exact . foodGrams $ f)
            in X.concat [q, u, a, g]
    False -> let q = exact . qty $ f
                 (UnitNameAmt (Name u) _) = currUnit f
                 a = exact . foodGrams $ f
                 g = '(' `cons` a `append` (pack " g)")
                 qu = q `snoc` ' ' `append` u
             in qu `snoc` ' ' `append` g `snoc` '\n'

{-
data RefuseYield = RefuseYield Food
instance Render RefuseYield where
  render o (RefuseYield f) = case oneColumn o of
    True -> let r = label "Percent refuse" 
-}
