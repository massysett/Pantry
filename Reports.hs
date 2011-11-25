module Reports where

import Food
import Types
import qualified Prelude as P
import Prelude hiding (lookup)
import qualified Data.List as L
import Data.List hiding (lookup)
import Data.List.Split
import qualified Data.Map as M
import Data.Map hiding (map, (\\), null)
import Data.Maybe
import Data.Either
import Data.Text hiding (map, null, drop, length, unlines, transpose,
                         zip, replicate, foldr)
import qualified Data.Text as X
import Data.Ratio
import Data.Decimal
import Reports.Columns
import Reports.Types
import Reports.Render

label :: (Render a) => String -> ReportOpts -> a -> X.Text
label l o d = pack l `append` pack ": " `append` render o d

data QtyUnitProp = QtyUnitProp Food

{-
instance Render QtyUnitProp where
  render o (QtyUnitProp f) = txt where
    txt = if oneColumn o then oneCol else twoCol
    oneCol = X.unlines [qty, unn, una, wei, pr, yld, id] where
-}    
