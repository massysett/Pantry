module Pantry.Parser (getConveyor) where

import qualified Pantry.Tray as T
import qualified Control.Monad.Error as E
import Pantry.Food (Error)
import qualified Data.ByteString.Lazy as BS

{-
data Incoming = Incoming


parseIncoming :: BS.ByteString -> Incoming
parseIncoming = undefined
-}

getConveyor :: BS.ByteString
               -> T.Tray
               -> E.ErrorT Error IO T.Tray
getConveyor = undefined
