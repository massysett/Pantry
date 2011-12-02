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
data Opts f = Opts {
  -- Options for create, all, find, id
  ignoreCase :: Bool
  , invert :: Bool
  , matcher :: (Text -> Bool)
    
  , xformFood :: Xform
  , xformDb :: [Foods -> Db -> Db]

  , prepend :: Bool
  , append :: Bool
  , reports :: [Report f]
    
  , reportOpts :: ReportOpts
    
    -- Options for all, find, id
  , tagMap :: TagMap
  , keys :: [Key]
  , edit :: Bool
  , delete :: Bool

    -- Options for all, find
  , xformFoods :: Foods -> Foods
  }

newtype NewDb = NewDb { unNewDb :: Db }
newtype Output = Output { unOutput :: Text }
data OutputAndDb = OutputAndDb { odbNewDb :: Db
                               , odbOutput :: Output }
data Quit = Quit

data ResultWrap = WrappedNewDb NewDb
                  | WrappedOutput Output
                  | WrappedOutputAndDb OutputAndDb

class Result a where
  wrap :: a -> ResultWrap

instance Result NewDb where
  wrap = WrappedNewDb
instance Result Output where
  wrap = WrappedOutput
instance Result OutputAndDb where
  wrap = WrappedOutputAndDb

sendResult :: Handle -> Either Error ResultWrap -> IO ()
sendResult = undefined

data Done = Done | NotDone

processMessage :: Handle -> Db -> IO (Maybe Db)
processMessage = undefined

-- COMMANDS
find :: (Food -> Bool)
        -> Opts f
        -> Db
        -> Either Error OutputAndDb
find = undefined

all :: Opts f
       -> Db
       -> Either Error OutputAndDb
all = find (\_ -> True)

create :: Opts f
          -> Db
          -> Either Error OutputAndDb
create = undefined

id :: (Food -> Bool)
      -> Opts f
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
