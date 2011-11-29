module Reports.Blank (blank) where

import Reports.Types (Report, emptyRpt, body)
import Data.Text (pack)

blank :: Report f
blank = emptyRpt {body = b} where
  b _ _ _ = pack "\n"

