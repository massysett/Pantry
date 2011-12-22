module Pantry.Reports.Ingredients(ingredients) where

import Prelude(($), (.), Maybe(Just, Nothing), fmap)
import Pantry.Exact(exact)
import Data.Text(Text, unwords, pack, unlines)
import Pantry.Food ( Food, getIngr, currUnitName, getTags,
                     unIngr, TagName(TagName), getCurrUnit,
                     getQty )
import qualified Data.Map as M
import Data.Foldable(Foldable, foldr)

ingredients :: Food -> Text
ingredients = unlines
              . (foldr (:) [])
              . fmap showIngr
              . unIngr
              . getIngr

showIngr :: Food -> Text
showIngr f = unwords [q, u, n] where
  q = exact . getQty $ f
  u = exact . currUnitName . getCurrUnit $ f
  n = case M.lookup (TagName . pack $ "name") (getTags f) of
    Nothing -> pack "(no name)"
    (Just v) -> exact v
