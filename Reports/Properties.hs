module Reports.Properties (properties) where

import Reports.Types
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

exactRational :: Rational -> Text
exactRational r = num `snoc` '/' `append` denom where
  num = pack . show . numerator $ r
  denom = pack . show . denominator $ r

exactDecimal :: Decimal -> Text
exactDecimal = pack . show

exactQty :: Qty -> Text
exactQty (Qty q) = case q of
  (Left nn) -> exactRational . nonNegToRational $ nn
  (Right nnm) ->
    let d = exactDecimal . mixedDec $ nnm
        r = exactRational . mixedRatio $ nnm
    in d `snoc` ' ' `append` r

exactGrams :: Grams -> Text
exactGrams (Grams nn) = exactRational . nonNegToRational $ nn

instance Render QtyUnitAmt where
  render o (QtyUnitAmt f) = case oneColumn o of
    True -> let q = label "Quantity" (exactQty . qty $ f)
                (UnitNameAmt (Name unit) amt) = currUnit f
                u = label "Unit name" unit
                a = label "Unit amount" (exactGrams amt)
                g = label "Total weight" (exactGrams . foodGrams $ f)
            in X.concat [q, u, a, g]
    False -> let q = exactQty . qty $ f
                 (UnitNameAmt (Name u) _) = currUnit f
                 a = exactGrams . foodGrams $ f
                 g = '(' `cons` a `append` (pack " g)")
                 qu = q `snoc` ' ' `append` u
             in qu `snoc` ' ' `append` g `snoc` '\n'
                 
                

