module Pantry.Parser where

import qualified Pantry.Tray as T
import qualified Control.Monad.Error as E
import Pantry.Error(Error)
import qualified Pantry.Error as R
import Pantry.Radio.Messages ( Request, args )
import System.Console.OptParse.OptParse (
  OptDesc(OptDesc), ArgDesc(Flag, Single, Double, Variable),
  parseOptsArgs)
import qualified Data.Map as M
import qualified Pantry.Matchers as Matchers
import qualified Pantry.Conveyor as C
import qualified Pantry.Reports.Types as RT
import qualified Pantry.Food as F
import Control.Monad ((>=>))
import Pantry.Types ( fromStr, NonNegInteger, NonNegMixed )
import Pantry.Reports ( buildReportGroups, printReportGroups )
import qualified Pantry.Sorter as S
import qualified Pantry.Paths as P
import Control.Monad.Trans ( liftIO )
import Data.Text ( Text, pack, singleton, isPrefixOf, unpack )
import System.Console.MultiArg.Prim
import System.Console.MultiArg.Combinator
import System.Console.MultiArg.Option
import Data.Set ( Set )
import Data.Maybe ( catMaybes )

data Opts = Opts {
  sensitive :: Matchers.CaseSensitive,
  invert :: Bool,
  matcher :: Text -> Either Error (Text -> Bool),
  conveyor :: T.Tray -> E.ErrorT Error IO T.Tray,
  reportOpts :: RT.ReportOpts,
  tagMap :: S.TagMap }

defaultOpts :: Opts
defaultOpts = let
  win = Matchers.within (Matchers.CaseSensitive False)
  defaultWithin s = return (win s)
  in Opts { sensitive = Matchers.CaseSensitive False
          , invert = False
          , matcher = defaultWithin
          , conveyor = return
          , reportOpts = RT.defaultReportOpts
          , tagMap = M.empty }

type PP = (String, Set LongOpt -> ParserSE Opts Error ())

noArg :: Maybe Char
         -> String
         -> Set LongOpt
         -> ParserSE Opts Error ()
noArg mc s los = do
  let optL = Just $ do
        let lo = makeLongOpt . pack $ s
        (_, _, mt) <- matchApproxLongOpt lo los
        case mt of
          Nothing -> return ()
          (Just l) -> zero $ R.LongOptDoesNotTakeArgument lo
      optS = case mc of
        Nothing -> Nothing
        (Just c) -> Just $ shortNoArg (makeShortOpt c) >> return ()
  choice $ catMaybes [ optL, optS ]

oneArg :: Maybe Char
          -> String
          -> Set LongOpt
          -> ParserSE Opts Error Text
oneArg mc s los = let
  optL = Just $ do
    let lo = makeLongOpt . pack $ s
    (_, _, mt) <- matchApproxLongOpt lo los
    case mt of
      (Just t) -> return t
      Nothing -> nextArg
  optS = case mc of
    Nothing -> Nothing
    (Just c) -> Just $ do
      let so = makeShortOpt c
      (_, t) <- shortOneArg so
      return t
  in choice . catMaybes $ [optL, optS]

twoArg :: Maybe Char
          -> String
          -> Set LongOpt
          -> ParserSE Opts Error (Text, Text)
twoArg mc s los = let
  optL = Just $ do
    let lo = makeLongOpt . pack $ s
    (_, _, mt) <- matchApproxLongOpt lo los
    case mt of
      (Just a1) -> do
        a2 <- nextArg
        return (a1, a2)
      Nothing -> do
        a1 <- nextArg
        a2 <- nextArg
        return (a1, a2)
  optS = case mc of
    Nothing -> Nothing
    (Just c) -> Just $ do
      let so = makeShortOpt c
      (_, a1, a2) <- shortTwoArg so
      return (a1, a2)
  in choice . catMaybes $ [optL, optS]

ignoreCase :: PP
ignoreCase = (o, f) where
  o = "ignore-case"
  f set = do
    noArg (Just 'i') o set
    modifySt (\s -> s { sensitive = Matchers.CaseSensitive False })

caseSensitive :: PP
caseSensitive = (o, f) where
  o = "case-sensitive"
  f set = do
    noArg Nothing o set
    modifySt (\s -> s { sensitive = Matchers.CaseSensitive True })

invertOpt :: PP
invertOpt = (o, f) where
  o = "invert"
  f set = do
    noArg (Just 'v') o set
    modifySt (\s -> s { invert = True })

noInvert :: PP
noInvert = (o, f) where
  o = "no-invert"
  f set = do
    noArg Nothing o set
    modifySt (\s -> s { invert = False })

flipCase :: 
  Bool  -- ^ Invert matching behavior?
  -> (Text -> Either Error (Text -> Bool))
  -> Text -> Either Error (Text -> Bool)
flipCase b f s = case b of
  True -> do
    m <- f s
    return (\t -> not (m t))
  False -> f s

within :: PP
within = (o, f) where
  o = "within"
  f set = do
    noArg Nothing o set
    s <- getSt
    let newSt = s { matcher = newMatcher }
        newMatcher = flipCase (invert s)
                     raiseMatcher (Matchers.within (sensitive s))
    putSt newSt

raiseMatcher ::
  (Text -> Text -> Bool)
  -> Text -> Either Error (Text -> Bool)
raiseMatcher f s = Right $ f s

