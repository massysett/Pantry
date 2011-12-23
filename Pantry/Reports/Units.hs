module Pantry.Reports.Units(units) where

import qualified Data.Map as M
import Pantry.Food ( getUnits )
import Pantry.Exact(Exact(exact))
import Data.Text(pack, append, Text)
import qualified Data.Text as X
import Pantry.Food(Food)

units :: Food -> Text
units = X.unlines . map toLine . M.assocs . getUnits where
  toLine (n, a) = pack "    " `append` na `append` amt where
    na = exact n
    amt = pack " (" `append` exact a `append` pack " g)"
