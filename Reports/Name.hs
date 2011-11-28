module Reports.Name (name) where

import Prelude((.), ($), Maybe(Nothing, Just))
import Food(TagNameVal(TagNameVal), TagVal(TagVal),
            Name(Name), getTag)
import Reports.Types(Report, emptyRpt, body)
import Data.Text(snoc, pack)

name :: Report
name = emptyRpt {body = b} where
  b _ _ f = snoc n '\n' where
    n = case getTag t f of
      Nothing -> pack "(No name)"
      (Just (TagNameVal _ (TagVal v))) -> v
    t = Name . pack $ "name"

