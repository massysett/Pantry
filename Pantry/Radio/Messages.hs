module Pantry.Radio.Messages where

import Data.Serialize(Serialize(get, put), Get )
import qualified Pantry.Paths as P
import Data.Text ( Text )
import qualified Data.Text as X
import Data.Word ( Word8 )
import Control.Applicative ( (<*>), (<$>), (*>) )
import Data.Text.Encoding(encodeUtf8, decodeUtf8)

data Request = Request { clientCurrDir :: P.ClientDir
                       , progName :: Text
                       , args :: [Text] }
instance Serialize Request where
  get = do
    cd <- get
    pn <- get
    as <- get
    return $ Request cd (decodeUtf8 pn) (map decodeUtf8 as)
  put (Request d p as) = do
    put d
    put $ encodeUtf8 p
    put (map encodeUtf8 as)

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
