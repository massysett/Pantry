-- | Allows communication between the client and the server.
module Pantry.Radio where

import Pantry.Bag (Bag)
import Pantry.Tray (Tray, bagToTray, unOutput, output, trayToBag)
import qualified Control.Monad.Error as E
import qualified Pantry.Error as R
import System.IO (Handle, hClose, hPutStrLn, stderr)
import qualified Data.DList as DL
import qualified Data.Text as X
import qualified Pantry.Radio.Messages as M
import qualified Data.ByteString.Lazy as BS
import Data.Serialize ( encodeLazy )
import System.FilePath ((</>))
import qualified Data.List as L
import System.Environment ( getEnvironment )
import System.Directory ( getHomeDirectory )
import qualified Control.Exception as Ex
import qualified Network as N

-- | TODO sanity checking on environment variable?
pantryDir :: IO FilePath
pantryDir = do
  e <- getEnvironment
  case L.lookup "PANTRY_DIR" e of
    Nothing -> do
      d <- getHomeDirectory
      return $ d </> ".pantry"
    (Just d) -> return d

toServerSocketName :: IO FilePath
toServerSocketName = do
  d <- pantryDir
  return $ d </> "toServer"

toClientSocketName :: IO FilePath
toClientSocketName = do
  d <- pantryDir
  return $ d </> "toClient"

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
--
-- TODO have this catch any exceptions that may arise from the
-- hPut. Not much that can be done at this point so just send them to
-- stderr. The Conveyor should not be throwing any IO exceptions, so
-- do not try to catch anything there.
processBag :: Bag
              -> (Tray -> E.ErrorT R.Error IO Tray)
              -> IO (Maybe Bag)
processBag b f = do
  let t = bagToTray b
      acquire = socketToClient
      release r = case r of
        Nothing -> return ()
        (Just h) -> hClose h
      use r = case r of
        Nothing -> return $ Just b
        (Just h) -> do
          e <- E.runErrorT (f t)
          let (newBag, bs) = encodeConveyed t e
          BS.hPut h bs
          return newBag
  Ex.bracket acquire release use

-- | Given the result of a conveyor, return the appropriate ByteString
-- to send to the client and the appropriate Maybe Bag to return to
-- the session.
encodeConveyed :: Tray -- ^ Return this tray in a bag if the Either is an error
                  -> Either R.Error Tray
                  -> (Maybe Bag, BS.ByteString)
encodeConveyed t e = (mb, encodeLazy r) where
  r = M.Response { M.text = x, M.exitCode = c }
  (x, c, mb) = case e of
    (Left err) ->
      (R.showError err, M.Fail (R.errorCode err), trayToBag t)
    (Right g) ->
      let y = X.concat . DL.toList . unOutput . output $ g
      in (y, M.Success, trayToBag g)
  
