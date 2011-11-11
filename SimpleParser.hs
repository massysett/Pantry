module SimpleParser where

import OptParse
import Data.List
import Control.Monad.Trans.Error

data SimpleErr = SimpleErr String deriving Show

instance ParseErr SimpleErr where
  store = SimpleErr

instance Error SimpleErr where
  strMsg = SimpleErr

data ParsedOpt = ParsedFlag String
               | ParsedSingle String String
               | ParsedDouble String String String
               deriving (Show, Eq)

emptyParsedOpt :: [ParsedOpt]
emptyParsedOpt = []

posArgParser :: [(opts, String)] -> Either err [String]
posArgParser os = Right $ map snd os

flagParser :: String -- ^ Name to reprt in result
              -> [ParsedOpt]
              -> Either SimpleErr  [ParsedOpt]
flagParser s ps = Right $ ps ++ [ParsedFlag s]

singleParser :: String
                -> [ParsedOpt]
                -> String
                -> Either SimpleErr [ParsedOpt]
singleParser n ps s1 = Right $ ps ++ [ParsedSingle n s1]

doubleParser :: String
                -> [ParsedOpt]
                -> String
                -> String
                -> Either SimpleErr [ParsedOpt]
doubleParser n ps s1 s2 = Right $ ps ++ [ParsedDouble n s1 s2]

makeFlagOpt :: [Char] -- ^ Short option names
               -> [String] -- ^ Long option names
               -> String -- ^ Name of this option for the [ParsedOpt] list
               -> OptDesc [ParsedOpt] SimpleErr
makeFlagOpt ss ls n = OptDesc ss ls (Flag $ flagParser n)

makeSingleOpt :: [Char] -- ^ Short option names
                 -> [String] -- ^ Long option names
                 -> String -- ^ Name of this option for the [ParsedOpt] list
                 -> OptDesc [ParsedOpt] SimpleErr
makeSingleOpt ss ls n = OptDesc ss ls (Single $ singleParser n)

makeDoubleOpt :: [Char] -- ^ Short option names
                 -> [String] -- ^ Long option names
                 -> String -- ^ Name of this option for the [ParsedOpt] list
                 -> OptDesc [ParsedOpt] SimpleErr
makeDoubleOpt ss ls n = OptDesc ss ls (Double $ doubleParser n)

{-
data NamedOptDesc opt err = NamedOptDesc String (OptDesc opt err)
data CLItem opt err = ShortCLItem Char (NamedOptDesc opt err)
                    | LongCLItem String (NamedOptDesc opt err)

data CLGroup opt err = CLGroup [CLItem opt err]

toCLItems :: NamedOptDesc opt err
            -> [NamedOptDesc opt err]
            -> [CLItem opt err]
toCLItems no@(NamedOptDesc n (OptDesc ss ls a)) os = shorts ++ longs where
  shorts = map makeShort ss
  makeShort s = ShortCLItem s no
  longs = map makeLong uniqueLongs
  makeLong s = LongCLItem s no
  uniqueLongs = concatMap (flip uniquePrefixes allLongs) ls
  allLongs = filter (`elem` ls) (concatMap namedOptDescToLongs os)
  namedOptDescToLongs (NamedOptDesc _ (OptDesc _ ss _)) = ss

groupShorts :: [CLItem opt err] -> [CLGroup opt err]
groupShorts cs = map CLGroup grouped where
  grouped = groupBy equal cs
  equal (ShortCLItem _ _) (ShortCLItem _ _) = True
  equal _ _ = False

clGroupToStrings :: CLGroup opt err -> [String]
clGroupToStrings = undefined


shortPermutations :: [CLItem opt err] -> [[[CLItem opt err]]]
shortPermutations cs = filter (not . bad) combs where
  combs = combinations cs
  bad ls = any (notFlag) (init ls)
  notFlag (ShortCLItem _ (NamedOptDesc _ (OptDesc _ _ (Flag _)))) = False
  notFlag _ = True

uniquePrefixes :: String -> [String] -> [String]
uniquePrefixes s ss = filter notPrefix (tail . inits $ s) where
  notPrefix p = not . any (p `isPrefixOf`) $ ss

  
  


combinations :: [a] -> [[[a]]]
combinations = foldr prependToList [[[]]]

prependItem :: a -> [[a]] -> [[[a]]]
prependItem p [[]] = [[[p]]]
prependItem p ((i:is):os) = ((p:i:is):os):([p]:(i:is):os):[]

prependToList :: a -> [[[a]]] -> [[[a]]]
prependToList = concatMap . prependItem
-}
