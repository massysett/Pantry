module Parser where

import Data.Text(Text)
import Food(TagNameVal, PctRefuse, MixedGrams, Name,
            NutAmt, NutNameAmt, Matcher, Xform,
            Food, Error)
import Types(NonNegMixed)
import Reports.Types(Report, ReportOpts)
import Db(Db, Foods)
import Sorter(Key, TagMap)
import qualified Control.Monad.Error as E

data SetYield = NoChange
                | ClearYield
                | SetYield MixedGrams

data BaseOpts f = BaseOpts {
  -- Pattern control
  ignoreCase :: Bool
  , invert :: Bool
  , matcher :: (Text -> Bool)
    
  , xformFood :: Xform
  , xformDb :: [Foods -> Db -> Db]

  , prepend :: Bool
  , append :: Bool
  , reports :: [Report f]
    
  , reportOpts :: ReportOpts
  }

-- | Export and remove ingredients are handled by adding to the
-- xformDb list.
data AllFindIdOpts f = AllFindIdOpts {
  allFindIdBase :: BaseOpts f
  , tagMap :: TagMap
  , keys :: [Key]
  , edit :: Bool
  , delete :: Bool
  }

data AllFindOpts f = AllFindOpts {
  allFindBase :: AllFindIdOpts f
  , xformFoods :: Foods -> Foods
  }

-- COMMANDS
find :: (Food -> Bool)
        -> AllFindOpts f
        -> Db
        -> E.ErrorT Error IO Db
find = undefined

all :: AllFindOpts f
       -> Db
       -> E.ErrorT Error IO Db
all = find (\_ -> True)

create :: AllFindOpts f
          -> Db
          -> E.ErrorT Error IO Db
create = undefined

id :: 
