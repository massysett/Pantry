module Rounded (Rounded(rounded)) where

import Prelude(Integral, round, show, (.), Integer,
               RealFrac)
import Data.Text(Text, pack)
import Data.Ratio(Ratio)

class Rounded a where
  rounded :: a -> Text

instance (Integral a) => Rounded (Ratio a) where
  rounded = pack . show . roundI

roundI :: (RealFrac a) => a -> Integer
roundI = round

