{-# LANGUAGE CPP #-}
-- | Client to server communication.
--
-- base 4.3.1.0 has a System.Environment.getArgs that does not return
-- a Unicode string. Instead, it simply puts each octet into a
-- different Char. Thus its getArgs is broken on UTF-8 and nearly any
-- non-ASCII encoding. As a workaround I use
-- System.Environment.UTF8. The downside of this is that it requires
-- that the command line be encoded in UTF8, regardless of what the
-- default system encoding is.
--
-- Unlike base 4.3.1.0, base 4.4.0.0 actually returns a proper Unicode
-- string when you call System.Environment.getArgs. (base 4.3.1.0
-- comes with ghc 7.0.4; base 4.4.0.0 comes with ghc 7.2.) The string
-- is encoded depending on the default system locale. The only problem
-- is that System.Environment.UTF8 apparently simply uses
-- System.Environment.getArgs and then assumes that the string it
-- returns has not been decoded. In other words,
-- System.Environment.UTF8 assumes that System.Environment.getArgs is
-- broken, and when System.Environment.getArgs was fixed in base
-- 4.4.0.0, it likely will break System.Environment.UTF8.
--
-- One obvious solution to this problem is to find some other way to
-- get the command line that will not break when base is updated. But
-- it was not easy to find such a thing. The other libraries I saw on
-- hackage (as of January 6, 2012) had problems, such as breakage on
-- ghc 7.2. There is a package that has a simple interface to the UNIX
-- setlocale(3) function, but I'm not sure that what it returns easily
-- and reliably maps to character encodings that you can use with,
-- say, iconv.
--
-- So by use of Cabal and preprocessor macors, the code uses
-- utf8-string if base is less than 4.4, and uses
-- System.Environment.getArgs if base is at least 4.4.
--
-- The GHC bug is here:
--
-- http://hackage.haskell.org/trac/ghc/ticket/3309

module Pantry.Radio.Client (clientMain) where

import Pantry.Radio ( toServerSocketName, toClientSocketName,
                      Listener(Listener) )
import qualified Pantry.Radio.Messages as M
import qualified Network as N
import Pantry.Paths as P
#if MIN_VERSION_base(4,4,0)
import System.Environment ( getArgs, getProgName )
#else
import System.Environment.UTF8 ( getArgs, getProgName )
#endif
import qualified Data.Text.IO as TIO
import System.Exit ( exitSuccess, exitWith, ExitCode(ExitFailure) )
import System.IO (hSetBinaryMode, hClose)
import qualified Data.ByteString as BS
import Data.Serialize ( encode, decode )
import System.Directory ( removeFile )
import Data.Text ( pack )
import Control.Monad ( liftM, liftM3, void )
import qualified System.Posix.Signals as S

-- | Creates the client listening connection. Does not catch any
-- exceptions.
getListener :: IO Listener
getListener = do
  f <- toClientSocketName
  let port = N.UnixSocket f
  handleSignals
  l <- N.listenOn port
  return $ Listener l

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

-- | Makes an IO action that installs a signal handler for the given
-- signal that removes the toClient socket. The handler will re-raise
-- the signal after it removes the file.
makeHandler :: S.Signal -> IO ()
makeHandler s = void $ S.installHandler s h set where
  h = S.Catch f
  f = do
    sockName <- toClientSocketName
    removeFile sockName
    S.raiseSignal s
  set = Just S.fullSignalSet

-- | Makes an IO action that installs signal handlers for ABRT, HUP,
-- INT, TERM, PIPE.
handleSignals :: IO ()
handleSignals =
  makeHandler S.internalAbort
  >> makeHandler S.lostConnection
  >> makeHandler S.keyboardSignal
  >> makeHandler S.softwareTermination
  >> makeHandler S.openEndedPipe


clientMain :: IO ()
clientMain = do
  req <- createMessage
  l <- getListener
  sendMessage req
  resp <- receiveResponse l
  closeAct <- printResponse resp
  sockName <- toClientSocketName
  removeFile sockName
  closeAct
