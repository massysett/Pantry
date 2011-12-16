module Pantry.Parser (getConveyor) where

import qualified Pantry.Tray as T
import qualified Control.Monad.Error as E
import Pantry.Error(Error)
import Pantry.Radio.Messages ( Request )

getConveyor :: Request
               -> T.Tray
               -> E.ErrorT Error IO T.Tray
getConveyor = undefined
