module Parser (getConveyor) where

import qualified Tray as T
import qualified Control.Monad.Error as E
import Food (Food, Error)
import qualified Data.ByteString as BS

{-
data Incoming = Incoming


parseIncoming :: BS.ByteString -> Incoming
parseIncoming = undefined
-}

getConveyor :: BS.ByteString
               -> T.Tray
               -> E.ErrorT Error IO T.Tray
getConveyor = undefined
