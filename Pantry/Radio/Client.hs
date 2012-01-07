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
-- <http://hackage.haskell.org/trac/ghc/ticket/3309>

module Pantry.Radio.Client (clientMain) where

import Pantry.Radio ( toServerSocketName, toClientSocketName,
                      Listener(Listener),
                      installHandlers )
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
