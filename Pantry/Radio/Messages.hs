{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pantry.Radio.Messages where

import Data.Serialize(Serialize)
import qualified Pantry.Paths as P
import qualified Data.Text as X
import Data.Word ( Word8 )

data Request = Request { clientCurrDir :: P.ClientDir
                       , progName :: String
                       , args :: [String] }

data ExitCode = Success | Fail Word8

data Response = Response { text :: X.Text
                         , exitCode :: ExitCode }
