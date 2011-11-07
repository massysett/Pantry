module OptParse where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import qualified Control.Monad.Error as E
import Control.Monad.State
import Control.Monad.Trans.Error
import Control.Monad.Instances
import Control.Monad.Loops

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
  ambiguousLongOpt :: String -> [String] -> a
  badShortOpt :: Char -> a
  badEqualsOpt :: String -> String -> a
  insufficientArgs :: Either String Char
                      -> Int
                      -> [String]
                      -> a
  longOptWithoutName :: a

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

isStopper :: String -> Bool
isStopper s = s == "--"

parseArgs :: (ParseErr err, Error err)
             => CharOpts opts
             -> StringOpts opts
             -> [String]
             -> opts
             -> Either err (opts, [(opts, String)])
parseArgs co so ss op =
  let (ei, opts) = unwrapState st defaultSt
      defaultSt = ParseState { stOpts = op
                             , stPos = []
                             , stLeft = ss }
      st = unwrapET parser
      parser = parseArgsM co so
  in case ei of
    (Left err) -> Left err
    (Right ()) -> Right (stOpts opts, stPos opts)

unwrapState :: (ParseErr err, Error err)
               => State (ParseState opts) (Either err ())
               -> ParseState opts
               -> (Either err (), ParseState opts)
unwrapState = runState

unwrapET :: (ParseErr err, Error err)
            => ErrorT err (State (ParseState opts)) ()
            -> State (ParseState opts) (Either err ())
unwrapET = runErrorT

parseArgsM :: (ParseErr err, Error err)
              => CharOpts opts
              -> StringOpts opts
              -> ErrorT err (State (ParseState opts)) ()
parseArgsM co so = do
  st <- lift get
  if (null . stLeft $ st)
    then return ()
    else pickParser (head . stLeft $ st) co so

pickParser :: (ParseErr err, Error err)
              => String
              -> CharOpts opts
              -> StringOpts opts
              -> ErrorT err (State (ParseState opts)) ()
pickParser lead co so
  | isStopper lead = parseStopper
  | isLongOpt lead = parseLongOpt so
  | isShortOpt lead = parseShortOpt co
  | otherwise = parsePosArg

------------------------------------------------------------
------------------------------------------------------------
-- SHORT OPTION PARSING
------------------------------------------------------------
------------------------------------------------------------

parseShortOpt :: (ParseErr err, Error err)
                 => CharOpts opts
                 -> ErrorT err (State (ParseState opts)) ()
parseShortOpt co = do
  st <- lift get
  let curr = tail . head . stLeft $ st
      newLeft = tail $ stLeft st
  put $ st { stLeft = newLeft }
  innerParseShort co curr

innerParseShort :: (ParseErr err, Error err)
                   => CharOpts opts
                   -> [Char]       -- ^ The word with the option - prefix omitted
                   -> ErrorT err (State (ParseState opts)) ()
innerParseShort _ [] = return ()
innerParseShort co (o:os) = do
  let maybeArgDesc = M.lookup o co
  when (isNothing maybeArgDesc) (throwError (badShortOpt o))
  let ad = fromJust maybeArgDesc
  case ad of (Flag f) -> parseShortFlag f co os
             (Single f) -> parseShortSingle f o os
             (Double f) -> parseShortDouble f o os

parseShortFlag :: (ParseErr err, Error err)
                  => (opts -> opts)
                  -> CharOpts opts
                  -> [Char]
                  -> ErrorT err (State (ParseState opts)) ()
parseShortFlag f co os = do
  st <- lift get
  let opts = stOpts st
      newOpts = f opts
      newSt = st {stOpts = newOpts}
  lift $ put newSt
  case os of [] -> return ()
             _ -> innerParseShort co os

parseShortSingle :: (ParseErr err, Error err)
                    => (opts -> String -> opts)
                    -> Char
                    -> [Char]
                    -> ErrorT err (State (ParseState opts)) ()
parseShortSingle f o os = do
  st <- lift get
  let opts = stOpts st
  case os of
    [] -> do
      when (null $ stLeft st) (throwError (insufficientArgs (Right o) 1 []))
      let (a:as) = stLeft st
          newOpts = f opts a
          newSt = st { stOpts = newOpts
                     , stLeft = as }
      lift $ put newSt
      return ()
    word -> do
      let newOpts = f opts word
          newSt = st { stOpts = newOpts }
      lift $ put newSt
      return ()

parseShortDouble :: (ParseErr err, Error err)
                    => (opts -> String -> String -> opts)
                    -> Char
                    -> [Char]
                    -> ErrorT err (State (ParseState opts)) ()
parseShortDouble f o os = do
  st <- lift get
  let opts = stOpts st
  case os of
    [] -> do
      when (length (stLeft st) < 2)
        (throwError (insufficientArgs (Right o) 2 []))
      let (a:b:as) = stLeft st
          newOpts = f opts a b
          newSt = st { stOpts = newOpts
                     , stLeft = as }
      lift $ put newSt
      return ()
    word -> do
      when (null (stLeft st)) (throwError (insufficientArgs (Right o) 2 []))
      let (a:as) = stLeft st
          newOpts = f opts word a
          newSt = st { stOpts = newOpts
                     , stLeft = as }
      lift $ put newSt
      return ()

------------------------------------------------------------
------------------------------------------------------------
-- LONG OPTION PARSING
------------------------------------------------------------
------------------------------------------------------------

parseLongOpt :: (ParseErr err, Error err)
                => StringOpts opts
                -> ErrorT err (State (ParseState opts)) ()
parseLongOpt so = do
  st <- lift get
  let curr = head . stLeft $ st
      newLeft = tail . stLeft $ st
      newSt = st { stLeft = newLeft }
  lift $ put newSt
  let ei = breakLongWord curr
  case ei of
    (Left err) -> throwError err
    (Right broken) -> pickLongParser so broken

pickLongParser :: (ParseErr err, Error err)
                  => StringOpts opts
                  -> (String, Maybe String)
                  -> ErrorT err (State (ParseState opts)) ()
pickLongParser so p@(s, _) = do
  case (bestLongArgDesc so s) of
    (Left err) -> throwError err
    (Right desc) -> case desc of
      (Flag f) -> parseLongFlag f p
      (Single f) -> parseLongSingle f p
      (Double f) -> parseLongDouble f p

parseLongFlag :: (ParseErr err, Error err)
                 => (opts -> opts)
                 -> (String, (Maybe String))
                 -> ErrorT err (State (ParseState opts)) ()
parseLongFlag f (s, (Just a)) = throwError $ badEqualsOpt s a
parseLongFlag f (s, Nothing) = do
  st <- lift get
  let opts = stOpts st
      newOpts = f opts
      newSt = st { stOpts = newOpts }
  lift $ put newSt
  return ()

parseLongSingle :: (ParseErr err, Error err)
                   => (opts -> String -> opts)
                   -> (String, (Maybe String))
                   -> ErrorT err (State (ParseState opts)) ()
parseLongSingle f (s, (Just a)) = do
  st <- lift get
  let opts = stOpts st
      newOpts = f opts a
      newSt = st { stOpts = newOpts }
  lift $ put newSt
  return ()
parseLongSingle f (s, Nothing) = do
  st <- lift get
  let left = stLeft st
  when (length left < 1) (throwError $ insufficientArgs (Left s) 1 [])
  let a = head left
      opts = stOpts st
      newOpts = f opts a
      newSt = st { stOpts = newOpts
                 , stLeft = tail left }
  lift $ put newSt
  return ()

parseLongDouble :: (ParseErr err, Error err)
                   => (opts -> String -> String -> opts)
                   -> (String, Maybe String)
                   -> ErrorT err (State (ParseState opts)) ()
parseLongDouble f (s, (Just a1)) = do
  st <- lift get
  let left = stLeft st
  when (length left < 1) (throwError $ insufficientArgs (Left s) 2 [a1])
  let a2 = head left
      opts = stOpts st
      newOpts = f opts a1 a2
      newSt = st { stOpts = newOpts
                 , stLeft = tail left }
  lift $ put newSt
  return ()

parseLongDouble f (s, Nothing) = do
  st <- lift get
  let left = stLeft st
  when (length left < 2) (throwError $ insufficientArgs (Left s) 2 [])
  let a1 = head left
      a2 = left !! 1
      opts = stOpts st
      newOpts = f opts a1 a2
      newSt = st { stOpts = newOpts
                 , stLeft = drop 2 left }
  lift $ put newSt
  return ()

-- |Applied to a string that is a long option, such as "--hello=yes",
-- returns the long option name and the argument given, if any. For
-- example, for "--hello=yes", returns ("hello", Just "yes"). Returns
-- an error if there is no long option name (e.g. "--=yes").
breakLongWord :: (Error err, ParseErr err) =>
                 String ->
                 Either err (String, (Maybe String))
breakLongWord s = do
  let trimmed = drop 2 s
      (pre, suf) = break (== '=') trimmed
      arg = if (null suf) then Nothing else (Just $ tail suf)
  when (null pre) (E.throwError longOptWithoutName)
  return (pre, arg)

-- |Finds the ArgDesc that is the best match for a string. If there is
-- an exact match, use that. Otherwise, if there is exactly one option
-- that starts with the word given, use that. Otherwise, returns an
-- error.
bestLongArgDesc :: (ParseErr err)
                   => StringOpts opts
                   -> String
                   -> Either err (ArgDesc opts)
bestLongArgDesc so s
  | isJust exact = Right (fromJust exact)
  | length matches == 1 = Right (snd . head $ matches)
  | length matches == 0 = Left (badLongOpt s)
  | otherwise = Left (ambiguousLongOpt s (map fst matches))
    where
      exact = M.lookup s so
      matches = filter p $ M.assocs so
      p (n, _) = s `isPrefixOf` n

------------------------------------------------------------
------------------------------------------------------------
-- STOPPER PARSING
------------------------------------------------------------
------------------------------------------------------------
parseStopper :: (ParseErr err, Error err)
                => ErrorT err (State (ParseState opts)) ()
parseStopper = do
  st <- lift get
  let newLeft = tail $ stLeft st
      newSt = st { stLeft = newLeft }
  lift $ put newSt
  whileM_ moreArgs parsePosArg
  
moreArgs :: (Error err) => ErrorT err (State (ParseState opts)) Bool
moreArgs = lift get >>= return . not . null . stLeft

------------------------------------------------------------
------------------------------------------------------------
-- POS ARG PARSING
------------------------------------------------------------
------------------------------------------------------------
parsePosArg :: (ParseErr err, Error err)
               => ErrorT err (State (ParseState opts)) ()
parsePosArg = do
  st <- lift get
  let curr = head . stLeft $ st
      opts = stOpts st
      newPos = (stPos st) ++ [(opts, curr)]
      newLeft = tail . stLeft $ st
      newSt = st { stLeft = newLeft 
                 , stPos = newPos }
  lift . put $ newSt


parseCmd :: (ParseErr err)
            => [CmdDesc cmd opts posargs]
            -> opts -- ^ Default opts
            -> [String] -- ^ To parse
            -> Either err (cmd, opts, posargs)
parseCmd = undefined

