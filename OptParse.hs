module OptParse where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Trans.Error
import Control.Monad.Instances

data Command = Create
             | Find
             | Id
             | Move
               
data PantryArgs = PantryArgs {
  createArgs :: [String]
  , findArgs :: [String]
  }

data Opts = Opts {
  -- Create opts
  ignoreCase :: Bool
  , exactMatch :: Bool
    
    -- Find opts
  , edit :: Bool
  , keyAsc :: [String]
  }


class ParseErr a where
  badLongOpt :: String -> a
  badShortOpt :: Char -> a
  badEqualsOpt :: String -> String -> a
  insufficientArgs :: Either String Char
                      -> Int
                      -> [String]
                      -> a

data PosDesc opts posargs = NoPosArgs posargs
                         | PosArgs ([(opts, String)] -> posargs)
data ArgDesc opts = Flag (opts -> opts)
               | Single (opts -> String -> opts)
               | Double (opts -> String -> String -> opts)
data OptDesc opts = OptDesc [Char] [String] (ArgDesc opts)
data CmdDesc cmd opts posargs =
  CmdDesc
  String -- ^ Text of command name
  (String -> cmd) -- ^How to return command name
  [OptDesc opts]
  (PosDesc opts posargs)

optParse :: (ParseErr err) 
            => opts  -- ^ Default opts
            -> [CmdDesc cmd opts posargs] -- Command descriptions
            -> [String] -- ^ Args to parse
            -> Either (err) (cmd, opts, posargs)
optParse = undefined


-----------------------------
-----------------------------

type CharOpts opts = M.Map Char (ArgDesc opts)
type StringOpts opts = M.Map String (ArgDesc opts)

addCharOpt :: ArgDesc opts -> CharOpts opts -> Char -> CharOpts opts
addCharOpt = undefined

addStringOpt :: ArgDesc opts -> StringOpts opts -> String -> StringOpts opts
addStringOpt = undefined

addCharOpts :: CharOpts opts -> OptDesc opts -> CharOpts opts
addCharOpts = undefined

addStringOpts :: StringOpts opts -> OptDesc opts -> StringOpts opts
addStringOpts = undefined

addOptsToLookups :: CmdDesc cmd opts posargs
                    -> (CharOpts opts, StringOpts opts)
addOptsToLookups = undefined

data ParseState opts = ParseState { stOpts :: opts -- ^ Opts parsed so far
                               , stPos :: [(opts, String)] -- ^ Pos args so far
                               , stLeft :: [String] -- ^ Args to parse
                               }

isLongOpt :: String -> Bool
isLongOpt s = "--" `isPrefixOf` s

isShortOpt :: String -> Bool
isShortOpt s = "-" `isPrefixOf` s

parseArg :: (ParseErr err, Error err)
            => CharOpts opts
            -> StringOpts opts
            -> ErrorT err (State (ParseState opts)) ()
parseArg co so = do
  st <- lift get
  let lead = head $ stLeft st
  pickParser lead co so

pickParser :: (ParseErr err, Error err)
              => String
              -> CharOpts opts
              -> StringOpts opts
              -> ErrorT err (State (ParseState opts)) ()
pickParser lead co so
  | isLongOpt lead = parseLongOpt so
  | isShortOpt lead = parseShortOpt co
  | otherwise = parsePosArg

parseShortOpt :: (ParseErr err, Error err)
                 => CharOpts opts
                 -> ErrorT err (State (ParseState opts)) ()
parseShortOpt = undefined

innerParseShort :: (ParseErr err, Error err)
                   => CharOpts opts
                   -> [Char]       -- ^ The word with the option - prefix omitted
                   -> ErrorT err (State (ParseState opts)) [Char] -- ^ Remaining letters in this word to parse
innerParseShort _ [] = return []
innerParseShort co (o:os) = do
  st <- lift get
  let maybeArgDesc = M.lookup o co
      opts = stOpts st
  when (isNothing maybeArgDesc) (throwError (badShortOpt o))
  let ad = fromJust maybeArgDesc
  case ad of
    (Flag f) -> do
      let newOpts = f opts
          newSt = st {stOpts = newOpts}
      lift $ put newSt
      innerParseShort co os
    (Single f) -> case os of
      [] -> do
        when (null $ stLeft st) (throwError (insufficientArgs (Right o) 1 []))
        let (a:as) = stLeft st
            newOpts = f opts a
            newSt = st { stOpts = newOpts
                       , stLeft = as }
        lift $ put newSt
        return []
      word -> do
        let newOpts = f opts word
            newSt = st { stOpts = newOpts }
        lift $ put newSt
        return []
    (Double f) -> case os of
      [] -> do
        when (length (stLeft st) < 2) (throwError (insufficientArgs (Right o) 2 []))
        let (a:b:as) = stLeft st
            newOpts = f opts a b
            newSt = st { stOpts = newOpts }
        lift $ put newSt
        return []
      word -> do
        when (null (stLeft st)) (throwError (insufficientArgs (Right o) 2 []))
        let (a:as) = stLeft st
            newOpts = f opts word a
            newSt = st { stOpts = newOpts }
        lift $ put newSt
        return []

{-
parseCarryoverOpt :: (ParseErr err, Error err)
                     => CharOpts opts
                     -> ErrorT err (State (ParseState opts)) ()
parseCarryoverOpt co = do
  st <- lift get
  let (o:os) = fromJust $ shortOpts st
      maybeArgDesc = M.lookup o co
      opts = stOpts st
  when (isNothing maybeArgDesc) (throwError (badShortOpt o))
  let ad = fromJust maybeArgDesc
  case ad of
    (Flag f) -> do
      let newOpts = f o
          newSt = st {stOpts = newOpts}
      lift $ put newSt 
    (Single f) -> 
-}
  

parseLongOpt :: (ParseErr err)
                => StringOpts opts
                -> ErrorT err (State (ParseState opts)) ()
parseLongOpt = undefined

parsePosArg :: (ParseErr err) => ErrorT err (State (ParseState opts)) ()
parsePosArg = undefined

parseArgs :: (ParseErr err)
             => CharOpts opts
             -> StringOpts opts
             -> opts        -- ^ Default opts
             -> [String] -- ^ Command line arguments remaining
             -> Either err (opts, [(opts, String)]) -- ^ opts, posargs
parseArgs = undefined

parseCmd :: (ParseErr err)
            => [CmdDesc cmd opts posargs]
            -> opts -- ^ Default opts
            -> [String] -- ^ To parse
            -> Either err (cmd, opts, posargs)
parseCmd = undefined

