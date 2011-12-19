module Pantry.Reports.Paste(paste) where

import Prelude(show, (.), ($), maybe, return, id, fmap)
import Data.Text(Text, append, pack, snoc, replace, singleton,
                 empty, concat)
import Pantry.Types(NonNegInteger)
import Pantry.Food(Food, TagNamesVals(TagNamesVals), tags,
            units, UnitNamesAmts(UnitNamesAmts), foodId,
            TagVal(TagVal), unFoodId, Name(Name))
import Data.Map(lookup, keys)

paste :: Food -> Text
paste = printFood

printLine :: NonNegInteger -> Name -> Text
printLine i (Name n) = cmd `append` iTxt `append` unit where
  cmd = pack "pantry id "
  iTxt = pack . show $ i
  unit = pack " -x " `append` quoted `snoc` '\n'
  quoted = singleton '\'' `append` q `snoc` '\''
  q = replace (pack "\'") (pack "\'\\\'\'") n

printFood :: Food -> Text
printFood f = com `append` us `snoc` '\n' where
  com = pack "# " `append` n `snoc` '\n'
  (TagVal n) = maybe (TagVal empty) id $ do
    let (TagNamesVals m) = tags f
    name <- lookup (Name . pack $ "name") m
    return name
  us = concat . fmap (printLine . unFoodId $ i) $ ns
  ns = keys . (\(UnitNamesAmts m) -> m) . units $ f
  i = foodId f
