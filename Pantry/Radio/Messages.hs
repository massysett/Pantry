module Pantry.Radio.Messages where

import Data.Serialize(Serialize(get, put), Get)
import qualified Pantry.Paths as P
import qualified Data.Text as X
import Data.Word ( Word8 )
import Control.Applicative ( (<*>), (<$>), (*>) )
import Data.Text.Encoding(encodeUtf8, decodeUtf8)

data Request = Request { clientCurrDir :: P.ClientDir
                       , progName :: String
                       , args :: [String] }
instance Serialize Request where
  get = Request <$> get <*> get <*> get
  put (Request d p a) = put d *> put p *> put a

data ExitCode = Success | Fail Word8
instance Serialize ExitCode where
  put Success = put (0 :: Word8)
  put (Fail c) = put (1 :: Word8) *> put c
  get = do
    a <- (get :: Get Word8)
    case a of
      0 -> return Success
      1 -> Fail <$> get
      _ -> fail "could not read ExitCode"

data Response = Response { text :: X.Text
                         , exitCode :: ExitCode }
instance Serialize Response where
  put (Response t e) = put (encodeUtf8 t) *> put e
  get = Response <$> (get >>= return . decodeUtf8) <*> get
