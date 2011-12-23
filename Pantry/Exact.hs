module Pantry.Exact(Exact(exact)) where

import Data.Text(Text, snoc, append, pack)
import Data.Ratio(Ratio, numerator, denominator)
import Data.Decimal(DecimalRaw)

-- | Render a datatype exactly -- without rounding.
class Exact a where
  exact :: a -> Text

instance (Integral a) => Exact (Ratio a) where
  exact r
    | numerator r == 0 = num
    | denominator r == 1 = num
    | otherwise = num `snoc` '/' `append` denom where
      num = pack . show . numerator $ r
      denom = pack . show . denominator $ r

instance (Integral i) => Exact (DecimalRaw i) where
  exact = pack . show

instance Exact Text where
  exact = id

instance Exact Integer where
  exact = pack . show
