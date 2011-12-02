module Session where

import Commands
import Db
import Network

newtype Listener = Listener { unListener :: Socket }

socketFilename :: IO Filename
socketFilename = undefined

session :: IO ()
session = do
  f <- socketFilename
  let port = UnixSocket . unFilename $ f
  l <- listenOn port
  let listener = Listener l
  sessionLoop blankDb listener

sessionLoop :: Db
               -> Listener
               -> IO ()
sessionLoop d l = do
  (h, _, _) <- accept . unListener $ l
  r <- processMessage h d
  case r of
    Nothing -> return ()
    (Just newDb) -> sessionLoop newDb l
