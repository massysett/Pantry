module Commands where

import System.IO(Handle)
import Data.Text(Text)
import Food(TagNameVal, PctRefuse, MixedGrams, Name,
            NutAmt, NutNameAmt, Matcher, Xform,
            Food, Error, FoodId)
import Types(NonNegMixed)
import Reports.Types(Report, ReportOpts)
import Db(Db, Foods, Filename)
import Sorter(Key, TagMap)
import qualified Control.Monad.Error as E
import Data.Text(Text)


data SetYield = NoChange
                | ClearYield
                | SetYield MixedGrams

-- Use for create command
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

-- Export and remove ingredients are handled by adding to the
-- xformDb list.
-- Use for id command
data AllFindIdOpts f = AllFindIdOpts {
  allFindIdBase :: BaseOpts f
  , tagMap :: TagMap
  , keys :: [Key]
  , edit :: Bool
  , delete :: Bool
  }

-- Use for all and find commands
data AllFindOpts f = AllFindOpts {
  allFindBase :: AllFindIdOpts f
  , xformFoods :: Foods -> Foods
  }

newtype NewDb = NewDb { unNewDb :: Db }
newtype Output = Output { unOutput :: Text }
data OutputAndDb = OutputAndDb { odbNewDb :: Db
                               , odbOutput :: Output }
data Quit = Quit

data Done = Done | NotDone

processMessage :: Handle -> Db -> IO (Maybe Db)
processMessage = undefined

-- COMMANDS
find :: (Food -> Bool)
        -> AllFindOpts f
        -> Db
        -> Either Error OutputAndDb
find = undefined

all :: AllFindOpts f
       -> Db
       -> Either Error OutputAndDb
all = find (\_ -> True)

create :: BaseOpts f
          -> Db
          -> Either Error OutputAndDb
create = undefined

id :: (Food -> Bool)
      -> AllFindIdOpts f
      -> Db
      -> Either Error OutputAndDb
id = undefined

open :: Filename
        -> E.ErrorT Error IO NewDb
open = undefined

appendFile :: Filename
              -> Db
              -> E.ErrorT Error IO NewDb
appendFile = undefined

prependFile :: Filename
               -> Db
               -> E.ErrorT Error IO NewDb
prependFile = undefined

close :: NewDb
close = undefined

save :: Db -> E.ErrorT Error IO NewDb
save = undefined

quit :: Quit
quit = undefined

status :: Db -> Output
status = undefined

help :: Output
help = undefined

version :: Output
version = undefined

copyright :: Output
copyright = undefined

data FirstPosition = First | NotFirst FoodId

move :: FirstPosition
        -> FoodId
        -> [FoodId]
        -> Db
        -> Either Error Db
move = undefined
