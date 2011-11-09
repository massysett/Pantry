-- | A full-featured command line arguments parser. Features:
--
-- * Parses both short options (@-i@) and long options (@--ignore@)
--
-- * Allows short options to be combined with other short options (@ls
-- -lh@ rather than @ls -l -h@) and with arguments (@foocmd -oarg@
-- rather than @foocmd -o arg@ and even @foocmd -ioarg@ assuming that
-- @-i@ is an option that takes no arguments)
--
-- * Allows long option to be combined with its argument with an equal
-- sign (GNU style, like @head --lines=20@). However this is optional;
-- having the long option and the argument in separate words is also
-- permitted (like @head --lines 20@).
--
-- * Parses \"commands\". Examples include @git clone@ or @cvs up@,
-- where @clone@ and @up@ are the commands, respectively. Each command
-- might take different options and handle positional arguments
-- differently.
--
-- * Both commands and options can be abbreviated to the shortest
-- unambiguous abbreviation. For example, instead of @--ignore-case@
-- you could use @--igno@ or even @--i@ if either of those is
-- unambiguous.
--
-- * Options, option arguments, commands, and positional arguments are
-- all parsed using user-provided functions. For example, you can use
-- this to change option arguments to different datatypes (an Int,
-- say) or to make sure there is a particular number of positional
-- arguments. You could make sure some options only appear on the
-- command line once, or you can make implement an option that toggles
-- an option on and off.
--
-- * Full error reporting using Either and a user-defined error
-- object. Parsing stops when an error is found, and that error is
-- reported. The user can define her own errors, which can be used if
-- an error is encountered when parsing options, option arguments,
-- commands, or positional arguments.
--
-- * Positional arguments and options can be freely interspersed.
--
-- * Recognizes @--@ by itself and then stops parsing options. All
-- subsequent words are then treated as positional arguments, even if
-- they begin with dashes.
--
-- * Options can take zero, one, or two arguments.
--
-- Non-features / disadvantages:
--
-- * Complicated.
--
-- * Some parsers, like System.Console.GetOpt, automatically generate
-- help messages; this does not.
--
-- * Options must take a set number of arguments. Unlike
-- System.Console.GetOpt, you cannot specify that an option takes an
-- optional argument.
--
-- * Positional arguments and options may always be freely
-- interspersed; this cannot be turned off.
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

-- | Instances of the ParseErr class can be used with the 'parse'
-- function to report errors. In addition, ParseErr can be used to
-- report custom errors. For example, one of the argument processing
-- functions in 'ArgDesc' can refurn a Left err to indicate that the
-- user passed an incorrect value. This will terminate all further
-- processing and report the error. Though options are typically
-- preceded with one or two dashes, none of the arguments passed to
-- the ParseErr functions are passed with leading dashes.
class ParseErr a where
  -- | A long option string was not recognized.
  badLongOpt :: String -- ^ The bad input
                -> a
  
  -- | A long option string was short but has more than one match. For
  -- instance the user passed the string @--he@ as an option, but the
  -- program has options for both @--hello@ and @--head@.
  ambiguousLongOpt :: String -- ^ The string the user passed
                      -> [String] -- ^ Possible matches
                      -> a
                      
  -- | A short option character has no recognized matches.
  badShortOpt :: Char -- ^ Unrecognized character
                 -> a
  
  -- | Long options can be passed GNU style, where the option and the
  -- option argument appear in the same word, as in @--number=4@. This
  -- works when the option has one or two arguments. When an option
  -- argument is passed for an option that does not take any
  -- arguments, this function is called.
  badEqualsOpt :: String -- ^ The option name
                  -> String -- ^ The option argument the user passed
                  -> a
  
  -- | The user did not give enough option arguments.
  insufficientArgs :: Either String Char
                      -- ^ The option name (short or long)
                      -> Int
                      -- ^ The number of arguments expected
                      -> [String]
                      -- ^ Number of arguments actually received
                      -> a

  -- | Long option was passed, and it has an equal sign and an option
  -- argument, but no name (e.g. @--=yes@)
  longOptWithoutName :: a
  
  -- | An unrecognized command name was used.
  noMatchingCmd :: String -- ^ name of unrecognized command
                   -> a

  -- | A command name matches more than one possible command name, for
  -- instance the user passed @fin@ but there are commands named
  -- @find@ and @finagle@.
  ambiguousCmd :: String -- ^ The string the user passed
                  -> [String] -- ^ Possible matches
                  -> a

  -- | The user did not provide any command at all.
  noCmd :: a

  -- | The user passed a positional argument where one is not allowed
  -- (e.g. for a command for which positional arguments are not
  -- allowed).
  posArgsNotAllowed :: String -- ^ Positional argument the user gave
                       -> a

-- | Describes a positional argument. For example, in the command
-- @pantry find -i name hello@, the positional arguments are @name@
-- and @hello@ (@-i@ is an option, not a positional argument).
data PosDesc opts posargs err =
  -- | Use if this command does not accept any positional
  -- arguments. However you still must give a function that takes no
  -- arguments; this function is called when and will be suppled as
  -- one of the elements of the tuple that "parse" returns. (Possible
  -- change: have "parse" return a Nothing instead in cases like
  -- this?)
  NoPosArgs posargs

  -- | "parse" parses all the positional arguments after the rest of
  -- the command line is processed. You supply the function that will
  -- be called here. This way the function can see all the command
  -- line arguments rather than seeing them just one at a time. Each
  -- element of this is a tuple, with the first part of the tuple
  -- being the options that were selected at the time this positional
  -- argument appeared, and the second being the positional argument
  -- itself. This way the output of the function can depend on what
  -- the command line options were. The function should return Left
  -- err if there was some problem.
  | PosArgs ([(opts, String)] -> Either err posargs)

-- | Describes the arguments that an option accepts. For example, the
-- command @head@ takes an option named @-n@, which accepts a single
-- argument for the number of lines to print. Each of these
-- constructors takes one argument, which is a function. The first
-- argument of the function is always the command line options as they
-- existed before this argument is parsed. This allows each option
-- behave differently depending upon what options preceded it. For
-- instance, this could be used to implement toggling behavior. Next
-- the function accepts zero or more string arguments, with each
-- string representing a command-line argument for the option. Finally
-- each function returns an Either. Return Left err if there was some
-- problem with the parse, or a Right opts with the new state of the
-- command-line options.
data ArgDesc opts err = Flag (opts -> Either err opts)
               | Single (opts -> String -> Either err opts)
               | Double (opts -> String -> String -> Either err opts)

-- | Describes a command line option. For example, in the command line
-- @pantry find -i name Chex@, a command line option is @-i@.
data OptDesc opts err =
  OptDesc { -- | Short option names, without the leading dash.
            optShort :: [Char] 

            -- | Long option names, without the leading dashes.
          , optLong :: [String] 
          
            -- | Whether this option takes any arguments and, if so, how many.
          , optArgDesc :: (ArgDesc opts err)
          }

-- | Describes a command. For instance, from the command line @pantry
-- find -i name Chex@, the command is @find@.
data CmdDesc cmd opts posargs err =
  CmdDesc { -- | The command name, such as @find@.
            cmdName :: String
          
            -- | Takes the full name of the command, returns something
            -- to indicate which command was called.
          , cmdF :: (String -> cmd)
            
            -- | A list of all the options that the command takes.
          , cmdOpts :: [OptDesc opts err]
          
            -- | How to parse positional arguments for this command.
          , cmdPos :: PosDesc opts posargs err }

-- | Parse a command line. Presumably you got it from
-- System.Environment.getArgs. Do not include the program name as the
-- first string to parse (consistent with System.Environment.getArgs).
parse :: (ParseErr err, Error err)
         => [OptDesc opts err]
         -- ^ Global options. For example, in the command @pantry -i
         -- find name pretzels@, @-i@ is a global option because it
         -- appears before the command.
         
         -> [CmdDesc cmd opts posargs err]
         -- ^ All command descriptions
         
         -> opts
         -- ^ Default options. When the command line is parsed, each
         -- option receives the options that have already been
         -- parsed. The first option will receive this item.
         
         -> [String]
         -- ^ What to parse
         
         -> Either err (cmd, opts, posargs)
         -- ^ Left if an error occurred; Right if everything
         -- succeeded. The tuple has the command that was seen, the
         -- final state of the options item, and any positional
         -- arguments.

parse ods ds o ss = do
  let (gc, gs) = addOptsToLookups ods
  (opts, left) <- parseArgs StopParsing gc gs ss o
  parseCmds ds opts (map snd left)

-----------------------------
-----------------------------

type CharOpts opts err = M.Map Char (ArgDesc opts err)
type StringOpts opts err = M.Map String (ArgDesc opts err)

-- | Adds a single argument description to a map of CharOpts, which
-- will be triggered by the given char.
addCharOpt :: ArgDesc opts err
              -> CharOpts opts err
              -> Char
              -> CharOpts opts err
addCharOpt ad old c = M.insert c ad old

-- | Adds a single argument description to a map of StringOpts. 
addStringOpt :: ArgDesc opts err
                -> StringOpts opts err
                -> String
                -> StringOpts opts err
addStringOpt ad old s = M.insert s ad old

addCharOpts :: CharOpts opts err
               -> OptDesc opts err
               -> CharOpts opts err
addCharOpts os (OptDesc cs _ a) = foldl (addCharOpt a) os cs

addStringOpts :: StringOpts opts err
                 -> OptDesc opts err
                 -> StringOpts opts err
addStringOpts os (OptDesc _ ss a) = foldl (addStringOpt a) os ss

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
parseCmdDesc d ss o = parseArgs ResumeParsing co so ss o where
  (co, so) = addCmdToLookups d

-- | What to do when the args parser encounters a non-option argument?
-- Both of these options will properly respect a \"stopper\"
-- (@--@). The stopper will be parsed and then the remaining arguments
-- will be returned unparsed.
data AtNonOpt

     -- | Stop parsing, do not parse the non-option argument, and
     -- return the remaining arguments unparsed
     = StopParsing
     
     -- | Add the non-option argument to the list of remaining
     -- arguments that will be returned, then resume parsing at the
     -- next word
     | ResumeParsing

parseArgs :: (ParseErr err, Error err)
             => AtNonOpt
             -> CharOpts opts err
             -> StringOpts opts err
             -> [String]
             -> opts
             -> Either err (opts, [(opts, String)])
parseArgs at co so ss op =
  let (ei, opts) = runState st defaultSt
      defaultSt = ParseState { stOpts = op
                             , stPos = []
                             , stLeft = ss }
      st = runErrorT parser
      parser = parseArgsM at co so
  in case ei of
    (Left err) -> Left err
    (Right ()) -> Right (stOpts opts, stPos opts)
    
parseArgsM :: (ParseErr err, Error err)
              => AtNonOpt
              -> CharOpts opts err
              -> StringOpts opts err
              -> ErrorT err (State (ParseState opts)) ()
parseArgsM at co so = do
  st <- lift get
  if (null . stLeft $ st)
    then return ()
    else pickParser at (head . stLeft $ st) co so
  parseArgsM at co so

pickParser :: (ParseErr err, Error err)
              => AtNonOpt
              -> String
              -> CharOpts opts err
              -> StringOpts opts err
              -> ErrorT err (State (ParseState opts)) ()
pickParser at lead co so
  | isStopper lead = parseStopper
  | isLongOpt lead = parseLongOpt so
  | isShortOpt lead = parseShortOpt co
  | otherwise = case at of StopParsing -> parseRemainingPosArgs
                           ResumeParsing -> parseOnePosArg

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
parseLongFlag _ (s, (Just a)) = throwError $ badEqualsOpt s a
parseLongFlag f (_, Nothing) = do
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
parseLongSingle f (_, (Just a)) = do
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
  parseRemainingPosArgs
  
------------------------------------------------------------
------------------------------------------------------------
-- POS ARG PARSING
------------------------------------------------------------
------------------------------------------------------------
parseOnePosArg :: (ParseErr err, Error err)
               => ErrorT err (State (ParseState opts)) ()
parseOnePosArg = do
  st <- lift get
  let curr = head . stLeft $ st
      opts = stOpts st
      newPos = (stPos st) ++ [(opts, curr)]
      newLeft = tail . stLeft $ st
      newSt = st { stLeft = newLeft 
                 , stPos = newPos }
  lift . put $ newSt

parseRemainingPosArgs :: (ParseErr err, Error err)
                         => ErrorT err (State (ParseState opts)) ()
parseRemainingPosArgs = do
  st <- lift get
  let stillLeft = not . null . stLeft $ st
  if stillLeft then parseOnePosArg else (return ())

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
      exact = find pd cs
      pd (CmdDesc s _ _ _) = curr == s
      matches = filter isPre cs
      isPre (CmdDesc s _ _ _) = curr `isPrefixOf` s
      names = map (\(CmdDesc s _ _ _) -> s) matches

-- |Throws an error if applied to an Either which is Left.
-- Otherwise, does nothing.
throwOnLeft :: E.MonadError e m
              => Either e b
              -> m ()
throwOnLeft (Left e) = E.throwError e
throwOnLeft _ = return ()

-- |Extracts the Right value from an Either. Bombs if there is no Right value.
fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight: value is not a right."
fromRight (Right b) = b
