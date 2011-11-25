module Reports.Units where

import Data.Text hiding (map, replicate)
import qualified Data.Text as X
import Reports.Types
import Food
import Data.Map hiding (map)


-- | TODO add grams to units report
unitsRpt :: Report
unitsRpt = emptyRpt {body = b} where
  b _ _ f = X.concat . map toString $ (assocs m) where
    toString ((Name k), _) =
      (pack . replicate 3 $ ' ') `append` k `snoc` '\n'
    (UnitNamesAmts m) = allAvailUnits f

