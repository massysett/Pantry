module Pantry.Reports.Paste(paste) where

import Data.Text ( Text, append, snoc, pack, replace,
                   singleton )
import qualified Data.Text as X
import Pantry.Types ( )
import Pantry.Exact ( exact )
import qualified Pantry.Food as F
import qualified Data.Map as M

{- The paste report looks like this:

<hash> Name of food
pantry --id <ID number> --set-unit unit-name unit-value

The first line is the current unit of the food. Additional lines are
other available units. If an additional available unit is an exact
duplicate of the food's current unit, then that available unit is
skipped.

-}


paste :: F.Food -> Text
paste f = com `append` lns `snoc` '\n' where
  com = pack "# " `append` (exact n) `snoc` '\n'
  n = M.findWithDefault (F.TagVal . pack $ "(no name)")
      (F.TagName . pack $ "name") (F.getTags f)
  currUnit = let cu = F.getCurrUnit f
             in (F.currUnitName cu, F.currUnitAmt cu)
  lns = currUnitTxt `append` otherUnitsTxt
  currUnitTxt = printUnit (F.getFoodId f) currUnit
  otherUnitsTxt = X.concat
                  . map (printUnit (F.getFoodId f))
                  . filter ((/=) currUnit)
                  . M.assocs
                  . F.getUnits
                  $ f

printUnit :: F.FoodId -> (F.UnitName, F.UnitAmt) -> Text
printUnit i (n, a) = X.concat [
  pack "pantry --id "
  , exact i
  , pack " --set-unit "
  , quoted . exact $ n
  , singleton ' '
  , quoted . exact $ a
  , singleton '\n'
  ]

quoted :: Text -> Text
quoted t = singleton '\'' `append` q `snoc` '\'' where
  q = replace (pack "\'") (pack "\'\\\'\'") t
