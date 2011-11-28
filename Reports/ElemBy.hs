module Reports.ElemBy (elemBy) where

import Prelude (Bool(True, False), foldr)

elemBy :: (a -> Bool) -> [a] -> Bool
elemBy f = foldr g False where
  g _ True = True
  g a False = f a
