module OptParse where

import Data.Map as M
import Control.Monad.State

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


data PosDesc opt posargs = NoPosArgs posargs
                         | PosArgs ([(opt, String)] -> posargs)
data ArgDesc b = Flag (b -> b)
               | Single (b -> String -> b)
               | Double (b -> String -> String -> b)
data OptDesc b = OptDesc [Char] [String] (ArgDesc b)
data CmdDesc cmd opts posargs =
  CmdDesc
  String -- ^ Text of command name
  (String -> cmd) -- ^How to return command name
  [OptDesc opts]
  (PosDesc opts posargs)

optParse :: [CmdDesc cmd opts posargs] -> [String] -> (cmd, opts, posargs)
optParse = undefined


-----------------------------
-----------------------------

type CharOpts a = M.Map Char (ArgDesc a)
type StringOpts a = M.Map String (ArgDesc a)

addCharOpt :: ArgDesc a -> CharOpts a -> Char -> CharOpts a
addCharOpt = undefined

addStringOpt :: ArgDesc a -> StringOpts a -> String -> StringOpts a
addStringOpt = undefined

addCharOpts :: CharOpts a -> OptDesc a -> CharOpts a
addCharOpts = undefined

addStringOpts :: StringOpts a -> OptDesc a -> StringOpts a
addStringOpts = undefined

data ParseState o = ParseState { opts :: o -- ^ Opts parsed so far
                               , pos :: [(o, String)] -- ^ Pos args so far
                               , left :: [String] -- ^ Args to parse
                               }

parseArg :: CharOpts a
            -> StringOpts a
            -> State (ParseState o) ()
parseArg = undefined

parseArgs :: CharOpts a
             -> StringOpts a
             -> a        -- ^ Default opts
             -> [String] -- ^ Command line arguments remaining
             -> (a, [(a, String)]) -- ^ opts, posargs
parseArgs = undefined


parseCmd :: [CmdDesc cmd opts posargs]
            -> opts -- ^ Default opts
            -> [String] -- ^ To parse
            -> (cmd, opts, posargs)
parseCmd = undefined
