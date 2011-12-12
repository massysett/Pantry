module Pantry.Session where

import Pantry.Tray
import Pantry.Bag
import Network ( PortID(UnixSocket), listenOn,
                 accept, Socket )
import System.IO ( hSetBinaryMode )
import qualified Data.ByteString.Lazy as BS
import Pantry.Parser ( getConveyor )

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
  hSetBinaryMode h True
  msg <- BS.hGetContents h
  let conveyor = getConveyor msg
  r <- processBag h b conveyor
  case r of
    Nothing -> return ()
    (Just newBag) -> sessionLoop newBag l

