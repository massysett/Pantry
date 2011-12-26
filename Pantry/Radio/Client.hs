-- | Client to server communication.

module Pantry.Radio.Client (clientMain) where

import Pantry.Radio ( toServerSocketName, toClientSocketName,
                      Listener(Listener) )
import qualified Pantry.Radio.Messages as M
import qualified Network as N
import Pantry.Paths as P
import System.Environment ( getArgs, getProgName )
import qualified Data.Text.IO as TIO
import System.Exit ( exitSuccess, exitWith, ExitCode(ExitFailure) )
import System.IO (hSetBinaryMode, hClose)
import qualified Data.ByteString as BS
import Data.Serialize ( encode, decode )

-- | Creates the client listening connection. Does not catch any
-- exceptions.
getListener :: IO Listener
getListener = do
  f <- toClientSocketName
  let port = N.UnixSocket f
  l <- N.listenOn port
  return $ Listener l

-- | Creates the message to send to the server.
createMessage :: IO M.Request
createMessage = do
  d <- P.clientDir
  p <- getProgName
  a <- getArgs
  return M.Request { M.clientCurrDir = d
                   , M.progName = p
                   , M.args = a }

-- | Sends message to the server.
sendMessage :: M.Request -> IO ()
sendMessage r = do
  p <- toServerSocketName
  let port = N.UnixSocket p
  h <- N.connectTo p port
  hSetBinaryMode h True
  let bs = encode r
  BS.hPut h bs
  hClose h

-- | Receives a response from the server.
receiveResponse :: Listener -> IO M.Response
receiveResponse (Listener s) = do
  (h, _, _) <- N.accept s
  hSetBinaryMode h True
  c <- BS.hGetContents h
  hClose h
  case decode c of
    (Right g) -> return g
    (Left e) -> error $ "pantry: error: could not decode message "
                ++ "from server. Error message given: " ++ e

-- | Prints the response received from the server. Exits when done.
printResponse :: M.Response -> IO ()
printResponse r = do
  TIO.putStr . M.text $ r
  case M.exitCode r of
    M.Success -> exitSuccess
    (M.Fail c) -> exitWith (ExitFailure (fromIntegral c))

clientMain :: IO ()
clientMain = do
  req <- createMessage
  l <- getListener
  sendMessage req
  resp <- receiveResponse l
  printResponse resp
