module Parser where

import Data.Text(Text)
import Food(TagNameVal, PctRefuse, MixedGrams, Name,
            NutAmt, NutNameAmt, Matcher, Xform)
import Types(NonNegMixed)
import Reports.Types(Report)

data SetYield = NoChange
                | ClearYield
                | SetYield MixedGrams

data BaseOpts f = BaseOpts {
  -- Pattern control
  ignoreCase :: Bool
  , invert :: Bool
  , matcher :: (Text -> Bool)
    
  , xformers :: [Xform]
  , prepend :: Bool
  , append :: Bool
  , reports :: [Report f]
  }
