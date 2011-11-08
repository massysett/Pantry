module OptParse ( ParseErr(..)
                , PosDesc(..)
                , ArgDesc(..)
                , OptDesc(..)
                , CmdDesc(..)
                , parse
                ) where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import qualified Control.Monad.Error as E
import Control.Monad.State
import Control.Monad.Trans.Error
import Control.Monad.Instances
import Control.Monad.Loops

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
  noMatchingCmd :: String -> a
  ambiguousCmd :: String -> [String] -> a
  noCmd :: a
  posArgsNotAllowed :: String -> a

data PosDesc opts posargs err =
  NoPosArgs posargs
  | PosArgs ([(opts, String)] -> Either err posargs)

data ArgDesc opts err = Flag (opts -> Either err opts)
               | Single (opts -> String -> Either err opts)
               | Double (opts -> String -> String -> Either err opts)
data OptDesc opts err = OptDesc [Char] [String] (ArgDesc opts err)

data CmdDesc cmd opts posargs err =
  CmdDesc { cmdName :: String
          , cmdF :: (String -> cmd)
          , cmdOpts :: [OptDesc opts err]
          , cmdPos :: PosDesc opts posargs err }

parse :: (ParseErr err, Error err)
         => [OptDesc opts err] -- ^ Global opts
         -> [CmdDesc cmd opts posargs err]
         -> opts -- ^ Default opts
         -> [String] -- ^ To parse
         -> Either err (cmd, opts, posargs)
parse ods ds o ss = do
  let (gc, gs) = addOptsToLookups ods
  (opts, left) <- parseArgsNoPosArgs gc gs ss o
  parseCmds ds opts left

-----------------------------
-----------------------------

type CharOpts opts err = M.Map Char (ArgDesc opts err)
type StringOpts opts err = M.Map String (ArgDesc opts err)

addCharOpt :: ArgDesc opts err
              -> CharOpts opts err
              -> Char
              -> CharOpts opts err
addCharOpt ad old c = M.insert c ad old

addStringOpt :: ArgDesc opts err
                -> StringOpts opts err
                -> String
                -> StringOpts opts err
addStringOpt ad old s = M.insert s ad old

addCharOpts :: CharOpts opts err
               -> OptDesc opts err
               -> CharOpts opts err
addCharOpts os (OptDesc cs _ a) = foldl (addCharOpt a) M.empty cs

addStringOpts :: StringOpts opts err
                 -> OptDesc opts err
                 -> StringOpts opts err
addStringOpts os (OptDesc _ ss a) = foldl (addStringOpt a) M.empty ss

addOptsToLookups :: [OptDesc opts err]
                    -> (CharOpts opts err, StringOpts opts err)
addOptsToLookups os = (co, so) where
  co = foldl addCharOpts M.empty os
  so = foldl addStringOpts M.empty os

addCmdToLookups :: CmdDesc cmd opts posargs err
                   -> (CharOpts opts err, StringOpts opts err)
addCmdToLookups (CmdDesc _ _ os _) = addOptsToLookups os

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

parseCmdDesc :: (ParseErr err, Error err)
                => CmdDesc cmd opts posargs err
                -> [String] -- ^ To parse
                -> opts     -- ^ Default options
                -> Either err (opts, [(opts, String)])
parseCmdDesc d ss o = parseArgs co so ss o where
  (co, so) = addCmdToLookups d

parseArgs :: (ParseErr err, Error err)
             => CharOpts opts err
             -> StringOpts opts err
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
    
parseArgsNoPosArgs :: (ParseErr err, Error err)
                      => CharOpts opts err
                      -> StringOpts opts err
                      -> [String]
                      -> opts
                      -> Either err (opts, [String])
parseArgsNoPosArgs co so ss op =
  let (ei, opts) = runState st defaultSt
      defaultSt = ParseState { stOpts = op
                             , stPos = []
                             , stLeft = ss }
      st = runErrorT $ parseArgsNoPosArgsM co so
  in case ei of
    (Left err) -> Left err
    (Right left) -> Right (stOpts opts, left)


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
              => CharOpts opts err
              -> StringOpts opts err
              -> ErrorT err (State (ParseState opts)) ()
parseArgsM co so = do
  st <- lift get
  if (null . stLeft $ st)
    then return ()
    else pickParser (head . stLeft $ st) co so

pickParser :: (ParseErr err, Error err)
              => String
              -> CharOpts opts err
              -> StringOpts opts err
              -> ErrorT err (State (ParseState opts)) ()
pickParser lead co so
  | isStopper lead = parseStopper
  | isLongOpt lead = parseLongOpt so
  | isShortOpt lead = parseShortOpt co
  | otherwise = parsePosArg

parseArgsNoPosArgsM :: (ParseErr err, Error err)
                       => CharOpts opts err
                       -> StringOpts opts err
                       -> ErrorT err (State (ParseState opts)) [String]
parseArgsNoPosArgsM co so = do
  st <- lift get
  if ((null . stLeft $ st) || ((head . head . stLeft $ st) /= '-'))
    then return $ stLeft st
    else do
         pickParserNoPosArgs (head . stLeft $ st) co so
         return $ stLeft st

pickParserNoPosArgs :: (ParseErr err, Error err)
                       => String
                       -> CharOpts opts err
                       -> StringOpts opts err
                       -> ErrorT err (State (ParseState opts)) ()
pickParserNoPosArgs lead co so
  | isLongOpt lead = parseLongOpt so
  | isShortOpt lead = parseShortOpt co

------------------------------------------------------------
------------------------------------------------------------
-- SHORT OPTION PARSING
------------------------------------------------------------
------------------------------------------------------------

parseShortOpt :: (ParseErr err, Error err)
                 => CharOpts opts err
                 -> ErrorT err (State (ParseState opts)) ()
parseShortOpt co = do
  st <- lift get
  let curr = tail . head . stLeft $ st
      newLeft = tail $ stLeft st
  put $ st { stLeft = newLeft }
  innerParseShort co curr

innerParseShort :: (ParseErr err, Error err)
                   => CharOpts opts err
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
                  => (opts -> Either err opts)
                  -> CharOpts opts err 
                  -> [Char]
                  -> ErrorT err (State (ParseState opts)) ()
parseShortFlag f co os = do
  st <- lift get
  let opts = stOpts st
      ei = f opts
      newOpts = fromRight ei
      newSt = st {stOpts = newOpts}
  throwOnLeft ei
  lift $ put newSt
  case os of [] -> return ()
             _ -> innerParseShort co os

parseShortSingle :: (ParseErr err, Error err)
                    => (opts -> String -> Either err opts)
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
          ei = f opts a
          newOpts = fromRight ei
          newSt = st { stOpts = newOpts
                     , stLeft = as }
      throwOnLeft ei
      lift $ put newSt
      return ()
    word -> do
      let ei = f opts word
          newOpts = fromRight ei
          newSt = st { stOpts = newOpts }
      throwOnLeft ei
      lift $ put newSt
      return ()

parseShortDouble :: (ParseErr err, Error err)
                    => (opts -> String -> String -> Either err opts)
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
          ei = f opts a b
          newOpts = fromRight ei
          newSt = st { stOpts = newOpts
                     , stLeft = as }
      throwOnLeft ei
      lift $ put newSt
      return ()
    word -> do
      when (null (stLeft st)) (throwError (insufficientArgs (Right o) 2 []))
      let (a:as) = stLeft st
          ei = f opts word a
          newOpts = fromRight ei
          newSt = st { stOpts = newOpts
                     , stLeft = as }
      throwOnLeft ei
      lift $ put newSt
      return ()

------------------------------------------------------------
------------------------------------------------------------
-- LONG OPTION PARSING
------------------------------------------------------------
------------------------------------------------------------

parseLongOpt :: (ParseErr err, Error err)
                => StringOpts opts err
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
                  => StringOpts opts err
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
                 => (opts -> Either err opts)
                 -> (String, (Maybe String))
                 -> ErrorT err (State (ParseState opts)) ()
parseLongFlag f (s, (Just a)) = throwError $ badEqualsOpt s a
parseLongFlag f (s, Nothing) = do
  st <- lift get
  let opts = stOpts st
      ei = f opts
      newOpts = fromRight ei
      newSt = st { stOpts = newOpts }
  throwOnLeft ei
  lift $ put newSt
  return ()

parseLongSingle :: (ParseErr err, Error err)
                   => (opts -> String -> Either err opts)
                   -> (String, (Maybe String))
                   -> ErrorT err (State (ParseState opts)) ()
parseLongSingle f (s, (Just a)) = do
  st <- lift get
  let opts = stOpts st
      ei = f opts a
      newOpts = fromRight ei
      newSt = st { stOpts = newOpts }
  throwOnLeft ei
  lift $ put newSt
  return ()
parseLongSingle f (s, Nothing) = do
  st <- lift get
  let left = stLeft st
  when (length left < 1) (throwError $ insufficientArgs (Left s) 1 [])
  let a = head left
      opts = stOpts st
      ei = f opts a
      newOpts = fromRight ei
      newSt = st { stOpts = newOpts
                 , stLeft = tail left }
  throwOnLeft ei
  lift $ put newSt
  return ()

parseLongDouble :: (ParseErr err, Error err)
                   => (opts -> String -> String -> Either err opts)
                   -> (String, Maybe String)
                   -> ErrorT err (State (ParseState opts)) ()
parseLongDouble f (s, (Just a1)) = do
  st <- lift get
  let left = stLeft st
  when (length left < 1) (throwError $ insufficientArgs (Left s) 2 [a1])
  let a2 = head left
      opts = stOpts st
      ei = f opts a1 a2
      newOpts = fromRight ei
      newSt = st { stOpts = newOpts
                 , stLeft = tail left }
  throwOnLeft ei
  lift $ put newSt
  return ()

parseLongDouble f (s, Nothing) = do
  st <- lift get
  let left = stLeft st
  when (length left < 2) (throwError $ insufficientArgs (Left s) 2 [])
  let a1 = head left
      a2 = left !! 1
      opts = stOpts st
      ei = f opts a1 a2
      newOpts = fromRight ei
      newSt = st { stOpts = newOpts
                 , stLeft = drop 2 left }
  throwOnLeft ei
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
                   => StringOpts opts err
                   -> String
                   -> Either err (ArgDesc opts err)
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

------------------------------------------------------------
------------------------------------------------------------
-- COMMAND NAME PARSING
------------------------------------------------------------
------------------------------------------------------------

-- | Applied to a list of command descriptions, a set of default
-- options, and a list of strings to parse, returns either an error or
-- the resulting command, the resulting options, and the resulting
-- posargs.
parseCmds :: (ParseErr err, Error err)
            => [CmdDesc cmd opts posargs err]
            -> opts -- ^ Default opts
            -> [String] -- ^ To parse
            -> Either err (cmd, opts, posargs)
parseCmds cs defaultOpts ss = do
  when (null ss) (E.throwError noCmd)
  desc@(CmdDesc name fCmd _ pd) <- pickCmd cs ss
  let rest = tail ss
  (resultOpts, ps) <- parseCmdDesc desc rest defaultOpts
  let cmd = fCmd name
  case pd of
    (NoPosArgs n) -> if not . null $ ps
                   then E.throwError $ posArgsNotAllowed name
                   else return (cmd, resultOpts, n)
    (PosArgs f) -> do
      let ei = f ps
          posargs = fromRight ei
      throwOnLeft ei
      return (cmd, resultOpts, posargs)

-- |Given a list of CmdDesc and a list of command line arguments
-- waiting to be parsed, returns the appropriate CmdDesc for the
-- argument at the head of the list. Assumes there is at least one
-- argument in the list; blows up if there isn't one.
pickCmd :: (ParseErr err, Error err)
           => [CmdDesc cmd opts posargs err]
           -> [String] -- ^ To parse
           -> Either err (CmdDesc cmd opts posargs err)
pickCmd cs ss
  | isJust exact = Right $ fromJust exact
  | length matches == 1 = Right $ head matches
  | null matches = Left $ noMatchingCmd curr
  | otherwise = Left $ ambiguousCmd curr names
    where
      curr = head ss
      exact = find pred cs
      pred (CmdDesc s _ _ _) = curr == s
      matches = filter isPre cs
      isPre (CmdDesc s _ _ _) = curr `isPrefixOf` s
      names = map (\(CmdDesc s _ _ _) -> s) matches

-- |Throws an error if applied to an Either which is Left.
-- Otherwise, does nothing.
throwOnLeft ::(E.MonadError e m)
              => Either e b
              -> m ()
throwOnLeft (Left e) = E.throwError e
throwOnLeft _ = return ()

-- |Extracts the Right value from an Either. Bombs if there is no Right value.
fromRight :: Either a b -> b
fromRight (Left a) = error "fromRight: value is not a right."
fromRight (Right b) = b
