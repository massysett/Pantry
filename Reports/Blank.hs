module Reports.Blank where

import Reports.Types
import Data.Text

blank :: Report
blank = emptyRpt {body = b} where
  b _ _ _ = pack "\n"

