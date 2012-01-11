-- | Client to server communication.

module Pantry.Radio.Client (clientMain) where

import Pantry.Radio ( toServerSocketName, toClientSocketName,
                      Listener(Listener),
                      installHandlers )
import qualified Pantry.Radio.Messages as M
import qualified Network as N
import Pantry.Paths as P
import System.Console.MultiArg.GetArgs ( getArgs, getProgName )
import qualified Data.Text.IO as TIO
import System.Exit ( exitSuccess, exitWith, ExitCode(ExitFailure) )
import System.IO (hSetBinaryMode, hClose)
import qualified Data.ByteString as BS
import Data.Serialize ( encode, decode )
import System.Directory ( removeFile )
import Data.Text ( pack )
import Control.Monad ( liftM, liftM3 )
import Control.Exception ( bracket )

-- | Creates the client listening connection. Does not catch any
-- exceptions. Returns a pair, with the first String being the
-- filename of the socket, and the second being the Listener itself.
getListener :: IO (String, Listener)
getListener = do
  f <- toClientSocketName
  let port = N.UnixSocket f
  installHandlers f
  l <- N.listenOn port
  return $ (f, Listener l)

-- | Creates the message to send to the server.
createMessage :: IO M.Request
createMessage = liftM3 M.Request P.clientDir (fmap pack getProgName)
                (liftM (map pack) getArgs)

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

-- | Prints the response received from the server. Returns an IO
-- action that can be used to exit when done.
printResponse :: M.Response -> IO (IO ())
printResponse r = do
  TIO.putStr . M.text $ r
  case M.exitCode r of
    M.Success -> return exitSuccess
    (M.Fail c) -> return $ exitWith (ExitFailure (fromIntegral c))

-- | Runs the client. Creates a message to send to the server. Then
-- creates a listener to listen for the response. When it comes in,
-- print it, then clean up the listener file and exit.
--
-- A number of things can happen that can cause the client to exit
-- early. Originally I tried to write signal handlers to deal with
-- some of these problems. For example, if the user puts the client in
-- a pipeline to @head@ and writes only the first ten lines of the
-- client's output, UNIX will deliver the client a SIGPIPE. So I
-- figured, catch this signal and then clean up the client
-- file. However, the SIGPIPE is not getting through to this code at
-- all. It seems that the runtime system is catching the signals and
-- blocking them. GHC and the runtime system have options to suppress
-- the installation of signal handlers (see the GHC RTS docs) but they
-- do not seem to be having any effect.
--
-- It may be that the RTS simply blocks all SIGPIPE signals and that
-- there is nothing you can do about it:
--
-- <http://hackage.haskell.org/trac/ghc/ticket/4274>
--
-- Apparently what does happen with a SIGPIPE, though, is that writing
-- to the broken pipe will cause a ResourceVanished exception to be
-- thrown. It's not immediately apparent that this actually happens:
-- most uncaught IO exceptions get printed before the program
-- terminates, but it is likely that the RTS is suppressing the
-- printing of broken pipe errors:
--
-- <http://hackage.haskell.org/trac/ghc/ticket/4889>
--
-- However, even if the error does not get printed on abnormal
-- termination, it seems the exception is still thrown. Therefore, the
-- way to deal with this is through bracket. Hopefully the bracket
-- code will deal with other abnormal terminations, such as a Ctrl-C
-- or a SIGTERM. I believe Ctrl-C does cause an exception to get
-- thrown, so it should be dealt with here. I don't know what a
-- SIGTERM, SIGHUP, etc. do. Hopefully they cause exceptions to be
-- thrown.
--
-- This message and the patch it points to suggest that SIGTERM and SIGHUP might not be throwing exceptions:
--
-- <http://www.haskell.org/pipermail/cvs-ghc/2010-January/052371.html>
--
-- In addition, on GHC 7.0.4, it seems that sending the server a
-- SIGTERM does not throw an exception.

clientMain :: IO ()
clientMain = do
  req <- createMessage
  resp <- bracket
          getListener
          (\(f, _) -> removeFile f )
          (\(_, l) -> do
              sendMessage req
              receiveResponse l )
  closeAct <- printResponse resp
  closeAct
