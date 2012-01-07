-- | Server-side communication.

module Pantry.Radio.Server (
  getListener
  , getRequest
  , processBag
  , serverLogFileName
  , Listener
  ) where

import Pantry.Bag (Bag)
import Pantry.Tray (Tray, bagToTray, unOutput, output, trayToBag)
import qualified Control.Monad.Error as E
import qualified Pantry.Error as R
import System.IO (Handle, hClose, hPutStrLn, stderr,
                  hSetBinaryMode)
import qualified Data.DList as DL
import qualified Data.Text as X
import qualified Pantry.Radio.Messages as M
import Pantry.Radio ( toServerSocketName, toClientSocketName,
                      pantryDir, Listener(Listener))
import qualified Data.ByteString as BS
import Data.Serialize ( encode, decode )
import System.FilePath ((</>))
import qualified Control.Exception as Ex
import qualified Network as N
import qualified Pantry.Paths as P

-- | The name of the file where the server will put its log file. The
-- log file will go here only if the server is daemonized; otherwise
-- all error messages will to the stderr of the parent process.
serverLogFileName :: IO FilePath
serverLogFileName = do
  (pd, _) <- pantryDir
  return $ pd </> "server_log"

-- | Returns a socket that listens for requests from clients.  Does
-- not catch any exceptions; for now just let it crash if there are
-- any problems. Returns a pair, with the String being the filename to
-- the listener, and the Listener itself.
getListener :: IO (String, Listener)
getListener = do
  f <- toServerSocketName
  let port = N.UnixSocket f
  l <- N.listenOn port
  return $ (f, Listener l)

-- | Given a Listener, block until a Request comes in. Returns a Just
-- Request if a request comes in. If there is some sort of problem (IO
-- problem, or decoding error) returns Nothing. Does not throw any IO
-- exceptions; other exceptions are not caught.
getRequest :: Listener -> IO (Maybe M.Request)
getRequest (Listener l) = Ex.catch comp handler >>= req where
  comp = do
    (h, _, _) <- N.accept l
    hSetBinaryMode h True
    c <- BS.hGetContents h
    return $ Just c
  handler e = do
    hPutStrLn stderr $ "pantryd: error while receiving message "
      ++ "from client: " ++ show (e :: Ex.IOException)
    return Nothing
  req m = case m of
    Nothing -> return Nothing
    (Just bs) -> case decode bs of
      (Left err) -> do
        hPutStrLn stderr $ "pantryd: error while decoding message "
          ++ "from client: " ++ err
        return Nothing
      (Right r) -> return (Just r)

-- | Obtains a handle to talk to the client socket. If there is an IO
-- error, catches it, prints it to standard error, and returns IO
-- Nothing.
socketToClient :: IO (Maybe Handle)
socketToClient = do
  ei <- Ex.try $ do
    n <- toClientSocketName
    N.connectTo n (N.UnixSocket n)
  case ei of
    (Left err) -> do
      hPutStrLn stderr
        ("pantryd: could not connect to client: " ++
         show (err :: Ex.IOException))
      return Nothing
    (Right h) -> return $ Just h

-- | Carries out the IO of a Conveyor and sends the resulting text and
-- error code to the client, and determines what new bag should be
-- passed up to the session.
processBag :: Bag
              -> P.ClientDir
              -> (Tray -> E.ErrorT R.Error IO Tray)
              -> IO (Maybe Bag)
processBag b d f = do
  let t = bagToTray b d
      acquire = socketToClient
      release r = case r of
        Nothing -> return ()
        (Just h) -> hClose h
      use r = case r of
        Nothing -> return $ Just b
        (Just h) -> do
          hSetBinaryMode h True
          e <- E.runErrorT (f t)
          let (newBag, bs) = encodeConveyed t e
          Ex.catch (BS.hPut h bs)
            (\err -> hPutStrLn stderr
                   ("pantryd: error while sending message to client: "
                   ++ show (err :: Ex.IOException)))
          return newBag
  Ex.bracket acquire release use

-- | Given the result of a conveyor, return the appropriate ByteString
-- to send to the client and the appropriate Maybe Bag to return to
-- the session.
encodeConveyed :: Tray -- ^ Return this tray in a bag if the Either is an error
                  -> Either R.Error Tray
                  -> (Maybe Bag, BS.ByteString)
encodeConveyed t e = (mb, encode r) where
  r = M.Response { M.text = x, M.exitCode = c }
  (x, c, mb) = case e of
    (Left err) ->
      (R.showError err, M.Fail (R.errorCode err), trayToBag t)
    (Right g) ->
      let y = X.concat . DL.toList . unOutput . output $ g
      in (y, M.Success, trayToBag g)
  
