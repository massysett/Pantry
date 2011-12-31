module Pantry.Matchers where

import qualified Data.Text as X
import Data.Text ( Text, unpack, pack)
import qualified Data.Text.Encoding as XE
import qualified Text.Regex.TDFA as TDFA
import qualified Text.Regex.PCRE as PCRE
import qualified Text.Regex.Base.RegexLike as RL
import qualified Control.Monad.Error as E
import qualified Pantry.Error as R
import Data.Bits((.|.))

newtype CaseSensitive = CaseSensitive { sensitive :: Bool }

newtype EitherW b = EitherW { unEitherW :: Either String b }

instance Monad EitherW where
  return a = EitherW (Right a)
  (EitherW (Left s)) >>= _ = EitherW (Left s)
  (EitherW (Right a)) >>= f = f a
  fail s = EitherW (Left s)

tdfa :: CaseSensitive -> Text -> Either R.Error (X.Text -> Bool)
tdfa c regexStr = let
  ew = case RL.makeRegexOptsM comp exec (unpack regexStr) of
    (EitherW (Left s)) -> E.throwError (R.RegexComp . pack $ s)
    (EitherW (Right rx)) -> return (\x -> RL.matchTest rx . X.unpack $ x)
    where
      comp = RL.defaultCompOpt { TDFA.caseSensitive = sensitive c
                               , TDFA.newSyntax = True
                               , TDFA.lastStarGreedy = True }
      exec = RL.defaultExecOpt { TDFA.captureGroups = False }
   in ew

pcre :: CaseSensitive -> Text -> Either R.Error (X.Text -> Bool)
pcre c regexStr = case RL.makeRegexOptsM comp exec (unpack regexStr) of
    (EitherW (Left s)) -> E.throwError (R.RegexComp . pack $ s)
    (EitherW (Right rx)) -> return (\x -> RL.matchTest rx . XE.encodeUtf8 $ x)
    where
      comp = RL.defaultCompOpt .|. PCRE.compUTF8 .|. caseless
      caseless = case (sensitive c) of
        False -> PCRE.compCaseless
        True -> 0
      exec = RL.defaultExecOpt

within :: CaseSensitive -> Text -> X.Text -> Bool
within = txtMatch X.isInfixOf

exact :: CaseSensitive -> Text -> X.Text -> Bool
exact = txtMatch (==)

txtMatch :: (X.Text -> X.Text -> Bool)
            -> CaseSensitive
            -> Text
            -> X.Text -> Bool
txtMatch f c s t = pat `f` txt where
  txt = flipCase t
  pat = flipCase s
  flipCase = case (sensitive c) of
    True -> id
    False -> X.toCaseFold
