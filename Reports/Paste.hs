module Reports.Paste(paste) where

import Prelude(show, (.), ($), Integer,
               maybe, return, id, fmap)
import Reports.Types(Report, body, emptyRpt)
import Data.Text(Text, append, pack, snoc, replace, singleton,
                 empty, concat)
import Food(Food, Name(Name), TagNamesVals(TagNamesVals), tags,
            units, UnitNamesAmts(UnitNamesAmts), foodId,
            TagVal(TagVal))
import Data.Map(lookup, keys)

paste :: Report f
paste = emptyRpt { body = f } where
  f _ _ food = printFood food

printLine :: Integer -> Name -> Text
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
  us = concat . fmap (printLine i) $ ns
  ns = keys . (\(UnitNamesAmts m) -> m) . units $ f
  i = foodId f
