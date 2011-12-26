-- | Allows communication between the client and the server.
module Pantry.Radio (
  pantryDir
  , toServerSocketName
  , toClientSocketName
  ) where

import System.FilePath ((</>))
import qualified Data.List as L
import System.Environment ( getEnvironment )
import System.Directory ( getHomeDirectory )

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

