module Reports.Properties (properties) where

import Reports.Types (Report(..), emptyRpt,
                      ReportOpts(..))
import Reports.Render
import Data.Text
import qualified Data.Text as X
import Food
import Data.Ratio
import Types
import Data.Decimal
import Exact

properties :: Report
properties = emptyRpt { body = f } where
  f o _ f = render o (Properties f)

-- Multi column property report:
-- 2 1/2 cups (83 g)
-- %R: .14    Yield: 240 g
-- ID: 9898

-- Single column property report:
-- Quantity: 2 1/2
-- Unit name: cups
-- Unit amount: 34 g
-- Total weight: 85 g
-- Percent refuse: .14
-- Yield: 240 g
-- ID: 9898
data QtyUnitAmt = QtyUnitAmt Food

label :: String -> Text -> Text
label s t = pack s `append` pack ": " `append` t `snoc` '\n'

instance Render QtyUnitAmt where
  render o (QtyUnitAmt f) = case oneColumn o of
    True -> let q = label "Quantity" (exact . qty $ f)
                (UnitNameAmt (Name unit) amt) = currUnit f
                u = label "Unit name" unit
                a = label "Unit amount" (exact amt)
                g = label "Total weight" (exact . foodGrams $ f)
            in X.concat [q, u, a, g]
    False -> let q = exact . qty $ f
                 (UnitNameAmt (Name u) _) = currUnit f
                 a = exact . foodGrams $ f
                 g = '(' `cons` a `append` (pack " g)")
                 qu = q `snoc` ' ' `append` u
             in qu `snoc` ' ' `append` g `snoc` '\n'

data RefuseYield = RefuseYield Food
instance Render RefuseYield where
  render o (RefuseYield f) =
    let (PctRefuse p) = pctRefuse f
        yn = case recipeYield f of
          (Just g) -> exact g `append` (pack " g")
          Nothing -> pack "(none)"
        yl = label "Yield" yn
    in case oneColumn o of
      True -> let pl = label "Percent refuse" (exact p)
              in pl `append` yl
      False -> let pl = (pack "%R: ") `append` (exact p)
               in pl `append` (pack "     ") `append` yl

labelId :: Food -> Text
labelId = label "ID" . pack . show . foodId

data Properties = Properties Food
instance Render Properties where
  render o (Properties f) = q `append` r `append` i where
    q = render o (QtyUnitAmt f)
    r = render o (RefuseYield f)
    i = labelId f
