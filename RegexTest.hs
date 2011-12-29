module Main where

import Text.Regex.Base.RegexLike
import Text.Regex.TDFA.String
import Control.Monad.Error

badPattern = "[unclosed brace"

main = do
  let maybeRegex =
        makeRegexOptsM defaultCompOpt
        defaultExecOpt badPattern :: Maybe Regex

  -- This outputs "Bad, no regex", as expected
  putStrLn $ maybe "Good regex"
    (const "Bad, no regex") maybeRegex

  let eitherRegex =
        makeRegexOptsM defaultCompOpt
        defaultExecOpt badPattern :: Either String Regex
  
  -- This throws an exception, why? The whole point of
  -- using makeRegexOptsM is to not have exceptions.
  putStrLn $ either show (const "Good regex") eitherRegex
