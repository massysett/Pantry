module Pantry.Reports.Name (name) where

import Prelude((.), ($), Maybe(Nothing, Just))
import Pantry.Food(TagNameVal(TagNameVal), TagVal(TagVal), getTag, Food)
import Data.Text(snoc, pack, Text)
import Pantry.Types(Name(Name))

name :: Food -> Text
name f = snoc n '\n' where
  n = case getTag t f of
    Nothing -> pack "(No name)"
    (Just (TagNameVal _ (TagVal v))) -> v
  t = Name . pack $ "name"

