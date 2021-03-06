-- | Allows communication between the client and the server.
--
-- Here is how communication works. By default all files are in the
-- directory ~/.pantry, unless the PANTRY_DIR environment variable is
-- specified.
--
-- 1. The server is started. It creates a socket named @toServer@ upon
-- which the server will listen for incoming connections. The server
-- calls socket, bind, listen, and accept on this socket. accept
-- blocks until a connection comes in.
--
-- 2. The client is started. The client creates a socket named
-- @toClient@ where the client listens for the response from the
-- server. The client calls socket, bind, and listen on this
-- socket. The client does not yet call accept on this socket, though.
--
-- 3. The client calls connect on the @toServer@ socket and sends its
-- message. It then closes its connection to @toServer@.
--
-- 4. The client calls accept on @toClient@ and awaits its response.
--
-- 5. The server receives a connection through its previous call to
-- accept (which had blocked, awaiting a message). It reads the entire
-- message and closes connection (remember that the listen call on
-- @toServer@ is still active and listening; it just won't connect
-- until accept is called again).
--
-- 6. The server finishes preparing its response and connects to
-- @toClient@ and sends its response and the closes its connection to
-- @toClient@. The server then calls accept on @toServer@ and awaits
-- another connection.
--
-- 7. The client receives its response from its previous accept
-- call. It receives its response, prints it, and quits.
--
-- 8. Repeat from step 2.

module Pantry.Radio (
  pantryDir
  , toServerSocketName
  , toClientSocketName
  , PantryDirInfo(IsDefaultDir, NotDefaultDir)
  , Listener(Listener)
  , installHandlers
  ) where

import System.FilePath ((</>))
import qualified Data.List as L
import System.Environment ( getEnvironment )
import System.Directory ( getHomeDirectory, removeFile )
import qualified Network as N
import Control.Monad ( void )
import qualified System.Posix.Signals as S

data PantryDirInfo = IsDefaultDir | NotDefaultDir
newtype Listener = Listener N.Socket

-- TODO sanity checking on environment variable?

-- | Returns the directory where Pantry communications will take
-- place. Returns a pair; the first value is the directory, and the
-- second indicates whether this is the default Pantry directory or
-- not.
pantryDir :: IO (FilePath, PantryDirInfo)
pantryDir = do
  e <- getEnvironment
  case L.lookup "PANTRY_DIR" e of
    Nothing -> do
      d <- getHomeDirectory
      return (d </> ".pantry", IsDefaultDir)
    (Just d) -> return (d, NotDefaultDir)

toServerSocketName :: IO FilePath
toServerSocketName = do
  (d, _) <- pantryDir
  return $ d </> "toServer"

toClientSocketName :: IO FilePath
toClientSocketName = do
  (d, _) <- pantryDir
  return $ d </> "toClient"

-- | Installs signal handlers to remove a particular file. Note that
-- not all signals that Unix sends will be delivered to the program
-- that is running inside the Haskell runtime, as the GHC runtime will
-- catch some signals. It is known to catch and suppress SIGPIPE and
-- SIGINT. However as of GHC version 7.0.4 the runtime will not catch
-- SIGTERM; instead it delivers those, and they can be caught.
installHandlers :: String -> IO ()
installHandlers s = mapM_ (sigRemoveFile s)
                    [S.sigHUP, S.sigTERM]

-- | Installs a signal handler to remove the file given when the
-- signal is caught, and then re-raise the signal. 
sigRemoveFile :: String -> S.Signal -> IO ()
sigRemoveFile f s = void $ S.installHandler s act mask where
  act = S.CatchOnce (removeFile f >> S.raiseSignal s)
  mask = Just S.fullSignalSet
