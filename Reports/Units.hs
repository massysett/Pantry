module Reports.Units(units) where

import Prelude((.), map)
import Data.Map(assocs)
import Reports.Types(Report(body), emptyRpt)
import qualified Food as F (units, UnitNamesAmts(UnitNamesAmts))
import Exact(Exact(exact))
import Data.Text(unlines, pack, append)

units :: Report
units = emptyRpt { body = b } where
  b _ _ = unlines . map toLine . assocs . toMap where
    toMap = (\(F.UnitNamesAmts m) -> m) . F.units
    toLine (n, a) = pack "    " `append` na `append` amt where
      na = exact n
      amt = pack " (" `append` exact a `append` pack " g)"
