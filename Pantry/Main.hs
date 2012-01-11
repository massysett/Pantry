module Pantry.Main ( main ) where

import System.Console.MultiArg.GetArgs ( getArgs, getProgName )
import Pantry.Session ( Opts (Opts), daemon, help, defaultOpts,
                        serverMain )
import Data.Text ( Text, pack, append )
import qualified Data.Text.IO as TIO
import System.Console.MultiArg.Prim
import System.Console.MultiArg.Combinator
import System.Console.MultiArg.Option
import System.Console.MultiArg.Error
import Control.Monad ( when )
import Control.Monad.Exception.Synchronous
import System.Exit
import Pantry.Radio.Client
import System.IO ( stderr )

data CmdLine = Server Opts
             | Client

cmdLine :: ParserSE () SimpleError CmdLine
cmdLine = server <|> client

server :: ParserSE () SimpleError CmdLine
server = do
  n <- nonOptionPosArg
  let e = unexpected (ExpTextError (pack "the word \"server\""))
          (SawTextError (pack "the word " `append` n))
  when (n /= pack "server") $ zero e
  opts

opts :: ParserSE () SimpleError CmdLine
opts = let
  fg = do
    _ <- mixedNoArg
         (makeLongOpt . pack $ "foreground")
         []
         [makeShortOpt 'f']
    return (\o -> o { daemon = False })
  he = do
    _ <- mixedNoArg
         (makeLongOpt . pack $ "help")
         []
         [makeShortOpt 'h']
    return (\o -> o { help = True })
  in do
    os <- manyTill (fg <|> he) end
    let opts = foldl (\o g -> g o) defaultOpts os
    return $ Server opts

client :: ParserSE () SimpleError CmdLine
client = return Client
    
main :: IO ()
main = do
  pn <- fmap pack getProgName
  as <- getArgs
  let argsText = map pack as
      ex = runParserSE () argsText cmdLine
  case ex of
    (Exception e) -> do
      TIO.hPutStrLn stderr (printError e)
      exitFailure
    (Success (g, _)) -> case g of
      (Server os) -> serverMain os
      Client -> clientMain pn argsText
