-- | Data types that have a Render instance that is universal across
-- report types can be defined here. Other Render instances should be
-- declared in the module in which they are used.
module Pantry.Reports.Render(Render(render)) where

import Pantry.Reports.Types
import Pantry.Types
import Pantry.Food
import Data.Text hiding (map, replicate)
import qualified Data.Map as M
import qualified Data.Text as X
import Pantry.Reports.Columns
import Pantry.Exact(Exact(exact))

class Render a where
  render :: ReportOpts -> a -> Text

instance Render UnitNameAmt where
  render _ (UnitNameAmt n g) = txt where
    txt = nt `append` open `append` gt `append` close
    nt = exact n
    open = pack " ("
    gt = exact g
    close = pack " g)"

instance Render UnitNamesAmts where
  render o (UnitNamesAmts m) = r where
    r = X.concat . map toLine . map toNameAmt . M.assocs $ m
    toLine u = blank `append` (render o u) `snoc` '\n' where
      blank = pack . replicate 4 $ ' '
    toNameAmt = uncurry UnitNameAmt

instance Render TagVal where
  render _ (TagVal t) = t

instance Render TagNameVal where
  render _ (TagNameVal n v) = r where
    r = lbl `append` col `append` val `snoc` '\n'
    lbl = exact n
    col = pack ": "
    val = exact v

instance Render PctRefuse where
  render o (PctRefuse b) = l `append` v where
    l | oneColumn o = pack "Percent refuse: "
      | otherwise = pack "%R: "
    v = exact b

instance Render TagNamesVals where
  render o (TagNamesVals m) = makeCols . makeLines $ m where
    makeCols | oneColumn o = X.concat
             | otherwise = newspaper [35]
    makeLines = map (render o)
                . map (uncurry TagNameVal)
                . M.assocs

instance Render NutRatio where
  render _ (NutRatio nn) =
    pack . show . roundI . (* 100) . nonNegToRational $ nn

roundI :: (RealFrac a) => a -> Integer
roundI = round
