module Matchers where

import qualified Data.Text as X
import qualified Data.Text.Encoding as XE
import qualified Text.Regex.TDFA as TDFA
import qualified Text.Regex.PCRE as PCRE
import qualified Data.ByteString as B
import qualified Text.Regex.Base.RegexLike as RL
import qualified Control.Monad.Error as E
import Food(Error(RegexComp))
import Data.Bits((.|.))

newtype CaseSensitive = CaseSensitive { sensitive :: Bool }

tdfa :: CaseSensitive -> String -> Either Error (X.Text -> Bool)
tdfa c regexStr = case RL.makeRegexOptsM comp exec regexStr of
  (Left s) -> E.throwError (RegexComp s)
  (Right rx) -> return (\x -> RL.matchTest rx . X.unpack $ x)
  where
    comp = RL.defaultCompOpt { TDFA.caseSensitive = sensitive c
                             , TDFA.newSyntax = True
                             , TDFA.lastStarGreedy = True }
    exec = RL.defaultExecOpt { TDFA.captureGroups = False }

pcre :: CaseSensitive -> String -> Either Error (X.Text -> Bool)
pcre c regexStr = case RL.makeRegexOptsM comp exec regexStr of
  (Left s) -> E.throwError (RegexComp s)
  (Right rx) -> return (\x -> RL.matchTest rx . XE.encodeUtf8 $ x)
  where
    comp = RL.defaultCompOpt .|. PCRE.compUTF8 .|. caseless
    caseless = case (sensitive c) of
      False -> PCRE.compCaseless
      True -> 0
    exec = RL.defaultExecOpt

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
