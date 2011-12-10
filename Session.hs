module Session where

import Commands
import Tray
import Bag
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
  sessionLoop emptyBag listener

sessionLoop :: Bag
               -> Listener
               -> IO ()
sessionLoop b l = do
  (h, _, _) <- accept . unListener $ l
  r <- processMessage h b
  case r of
    Nothing -> return ()
    (Just newBag) -> sessionLoop newBag l
