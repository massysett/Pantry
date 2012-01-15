module Pantry.Reports.Properties (properties) where

import Pantry.Reports.Types (ReportOpts(oneColumn))
import Data.Text(Text, pack, append, snoc, singleton)
import qualified Data.Text as X
import qualified Pantry.Food as F
import Pantry.Exact(Exact(exact))
import Pantry.Rounded ( rounded )

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

properties :: ReportOpts -> F.Food -> Text
properties o f = case (oneColumn o) of
  True -> oneColReport f
  False -> twoColReport f

label :: String -> Text -> Text
label s t = pack s `append` pack ": " `append` t

twoColReport :: F.Food -> Text
twoColReport f = X.unlines [
  let q = exact . F.getQty $ f
      u = exact . F.currUnitName . F.getCurrUnit $ f
      paren t = singleton '(' `append` t `append` singleton ')'
  in q
     `snoc` ' '
     `append` u
     `snoc` ' '
     `append` paren ((rounded . F.foodGrams $ f) `snoc` 'g')

  , let r = label "%R" (exact . F.getPctRefuse $ f)
        y = case F.getYieldGrams f of
          Nothing -> X.empty
          (Just yld) -> let
            yamt = rounded yld
            in label "    Yield" (yamt `append` singleton 'g')
    in r `append` y
       
  , label "ID" (exact . F.getFoodId $ f)
  ]

oneColReport :: F.Food -> Text
oneColReport f = X.unlines [
  label "Quantity" (exact . F.getQty $ f)
  , label "Unit name" (exact . F.currUnitName . F.getCurrUnit $ f)
  , label "Unit amount" ((exact . F.currUnitName . F.getCurrUnit $ f)
                         `append` (pack " g"))
  , label "Total weight" ((exact . F.foodGrams $ f) `append`
                          (pack " g"))
  , label "Percent refuse" (exact . F.getPctRefuse $ f)
  , label "Yield" (case F.getYieldGrams f of
                      Nothing -> pack "(none)"
                      (Just yld) -> ((rounded yld) `append` pack " g"))
  , label "ID" (exact . F.getFoodId $ f)
  ]

{-
properties :: ReportOpts -> Food -> Text
properties o f = case oneColumn o of
  
  
  
  True -> let q = label "Quantity" (exact . F.getQty $ f)
              (F.CurrUnit una uam) = F.currUnit f
              u = label "Unit name" (exact una)
              a = label "Unit amount" (exact uam)
              g = label "Total weight" (exact . F.foodGrams $ f)
          in concat [q, u, a, g]
  False -> let q = exact . qty $ f
               (UnitNameAmt (Name u) _) = currUnit f
               a = exact . F.foodGrams $ f
               g = '(' `cons` a `append` (pack " g)")
               qu = q `snoc` ' ' `append` u
           in qu `snoc` ' ' `append` g `snoc` '\n'


data RefuseYield = RefuseYield F.Food
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
-}
