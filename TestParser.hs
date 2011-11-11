module Main where

import SimpleParser
import OptParse
import System.Environment

optdescs = [ makeFlagOpt "f" ["flag"] "flag"
           , makeSingleOpt "s" ["single"] "single"
           ]

main :: IO ()
main = do
  as <- getArgs
  let r = parseOptsArgs optdescs emptyParsedOpt posArgParser as
      types = r :: Either SimpleErr ([ParsedOpt], [String])
  print r
