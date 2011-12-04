module Commands where

import System.IO(Handle)
import Data.Text(Text)
import Food(TagNameVal, PctRefuse, MixedGrams, Name,
            NutAmt, NutNameAmt, Matcher, Xform,
            Food, Error, FoodId)
import Types(NonNegMixed)
import Reports.Types
import Db
import Sorter(Key, TagMap)
import qualified Control.Monad.Error as E
import Data.Text(Text)


data SetYield = NoChange
                | ClearYield
                | SetYield MixedGrams

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

xformToTray :: (Food -> Either Error Food)
               -> (Tray -> E.ErrorT Error IO Tray)
xformToTray f = g where
  g t = do
    fs <- liftToErrorT $ mapM f (volatile t)
    return t { volatile = fs }

