module Pantry.FoodFns where

import Pantry.Food

changeCurrUnits :: (T.Traversable t)
                   => (Text -> Bool) -> t Food -> Either R.Error (t Food)
changeCurrUnits m = T.mapM (changeCurrUnit m)

-- Nut manipulations
addNut :: NutNameAmt -> Food -> Either R.Error Food
addNut (NutNameAmt n a) f = if' notZero (Right newFood) (Left err) where
  g = foodGrams f
  notZero = g /= zero
  newFood = f {nutsPerGs = newPerGs}
  (NutNamesPerGs oldPerGs) = nutsPerGs f
  newPerG = NutsPerG . fromJust $ rat `divide` gr where
    (NutAmt rat) = a
    (Grams gr) = g
  newPerGs = NutNamesPerGs $ M.insert n newPerG oldPerGs
  err = R.AddNutToZeroQty

addNuts :: (F.Foldable f) => f NutNameAmt -> Food -> Either R.Error Food
addNuts ns f = F.foldlM (flip addNut) f ns

addNutsToFoods :: (F.Foldable a, T.Traversable t)
                  => a NutNameAmt
                  -> t Food
                  -> Either R.Error (t Food)
addNutsToFoods ns = T.mapM (addNuts ns)

