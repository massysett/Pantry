module Pantry.Reports.Units(units) where

import Prelude((.), map)
import Data.Map(assocs)
import qualified Pantry.Food as F (units, UnitNamesAmts(UnitNamesAmts))
import Pantry.Exact(Exact(exact))
import Data.Text(unlines, pack, append, Text)
import Pantry.Food(Food)

units :: Food -> Text
units = unlines . map toLine . assocs . toMap where
  toMap = (\(F.UnitNamesAmts m) -> m) . F.units
  toLine (n, a) = pack "    " `append` na `append` amt where
    na = exact n
    amt = pack " (" `append` exact a `append` pack " g)"
