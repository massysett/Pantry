-- | Allows communication between the client and the server.
module Pantry.Radio where

import Pantry.Bag (Bag)
import Pantry.Tray (Tray)
import qualified Control.Monad.Error as E
import qualified Pantry.Error as R
import System.IO (Handle)

processBag :: Handle
              -> Bag
              -> (Tray -> E.ErrorT R.Error IO Tray)
              -> IO (Maybe Bag)
processBag = undefined

