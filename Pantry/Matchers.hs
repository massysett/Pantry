module Pantry.Matchers where

import qualified Data.Text as X
import qualified Data.Text.Encoding as XE
import qualified Text.Regex.TDFA as TDFA
import qualified Text.Regex.PCRE as PCRE
import qualified Text.Regex.Base.RegexLike as RL
import qualified Control.Monad.Trans.Error as T
import qualified Data.Functor.Identity as I
import qualified Control.Monad.Error as E
import qualified Pantry.Error as R
import Data.Bits((.|.))

newtype CaseSensitive = CaseSensitive { sensitive :: Bool }

tdfa :: CaseSensitive -> String
        -> T.ErrorT R.Error I.Identity (X.Text -> Bool)
tdfa c regexStr = let
  et = case RL.makeRegexOptsM comp exec regexStr of
    (Left s) -> T.throwError (R.RegexComp s)
    (Right rx) -> return (\x -> RL.matchTest rx . X.unpack $ x)
    where
      comp = RL.defaultCompOpt { TDFA.caseSensitive = sensitive c
                               , TDFA.newSyntax = True
                               , TDFA.lastStarGreedy = True }
      exec = RL.defaultExecOpt { TDFA.captureGroups = False }
  in case I.runIdentity . T.runErrorT $ et of
    (Left s) -> E.throwError s
    (Right rx) -> return rx

{-
tdfa :: CaseSensitive -> String -> Either R.Error (X.Text -> Bool)
tdfa c regexStr = case RL.makeRegexOptsM comp exec regexStr of
  (Left s) -> E.throwError (R.RegexComp s)
  (Right rx) -> return (\x -> RL.matchTest rx . X.unpack $ x)
  where
    comp = RL.defaultCompOpt { TDFA.caseSensitive = sensitive c
                             , TDFA.newSyntax = True
                             , TDFA.lastStarGreedy = True }
    exec = RL.defaultExecOpt { TDFA.captureGroups = False }

-}

pcre :: CaseSensitive -> String -> Either R.Error (X.Text -> Bool)
pcre c regexStr = let
  et = case RL.makeRegexOptsM comp exec regexStr of
    (Left s) -> T.throwError (R.RegexComp s)
    (Right rx) -> return (\x -> RL.matchTest rx . XE.encodeUtf8 $ x)
    where
      comp = RL.defaultCompOpt .|. PCRE.compUTF8 .|. caseless
      caseless = case (sensitive c) of
        False -> PCRE.compCaseless
        True -> 0
      exec = RL.defaultExecOpt
  in case I.runIdentity . T.runErrorT $ et of
    (Left s) -> E.throwError s
    (Right rx) -> return rx

{-
pcre :: CaseSensitive -> String -> Either R.Error (X.Text -> Bool)
pcre c regexStr = case RL.makeRegexOptsM comp exec regexStr of
  (Left s) -> E.throwError (R.RegexComp s)
  (Right rx) -> return (\x -> RL.matchTest rx . XE.encodeUtf8 $ x)
  where
    comp = RL.defaultCompOpt .|. PCRE.compUTF8 .|. caseless
    caseless = case (sensitive c) of
      False -> PCRE.compCaseless
      True -> 0
    exec = RL.defaultExecOpt
-}

within :: CaseSensitive -> String -> X.Text -> Bool
within = txtMatch X.isInfixOf

exact :: CaseSensitive -> String -> X.Text -> Bool
exact = txtMatch (==)

txtMatch :: (X.Text -> X.Text -> Bool)
            -> CaseSensitive
            -> String
            -> X.Text -> Bool
txtMatch f c s t = pat `f` txt where
  txt = flipCase t
  pat = flipCase . X.pack $ s
  flipCase = case (sensitive c) of
    True -> id
    False -> X.toCaseFold
