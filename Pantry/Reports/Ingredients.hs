module Pantry.Reports.Ingredients(ingredients) where

import Prelude(($), (.), Maybe(Just, Nothing), fmap)
import Pantry.Exact(exact)
import Data.Text(Text, unwords, pack, unlines)
import Pantry.Food(Food, ingr, UnitNameAmt(UnitNameAmt), currUnit,
            TagNameVal(TagNameVal), qty, getTag,
            Ingr(Ingr), Name(Name))
import Data.Foldable(Foldable, foldr)

ingredients :: Food -> Text
ingredients = unlines
              . (foldr (:) [])
              . fmap showIngr
              . (\(Ingr i) -> i)
              . ingr

showIngr :: Food -> Text
showIngr f = unwords [q, u, n] where
  q = exact . qty $ f
  u = exact . (\(UnitNameAmt a _) -> a) . currUnit $ f
  n = case getTag (Name . pack $ "name") f of
    Nothing -> pack "(no name)"
    (Just (TagNameVal _ v)) -> exact v
