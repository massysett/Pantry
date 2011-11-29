module Reports.Ingredients(ingredients) where

import Prelude(($), (.), Maybe(Just, Nothing), fmap)
import Exact(exact)
import Reports.Types(Report, body, emptyRpt)
import Data.Text(Text, unwords, pack, unlines)
import Food(Food, ingr, UnitNameAmt(UnitNameAmt), currUnit,
            Name(Name), TagNameVal(TagNameVal), qty, getTag,
            Ingr(Ingr))
import Data.Foldable(Foldable, foldr)

ingredients :: Report f
ingredients = emptyRpt { body = f } where
  f _ _ = unlines
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
