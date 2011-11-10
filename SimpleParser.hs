module SimpleParser where

import OptParse
import Data.List

data SimpleErr = SimpleErr String

instance ParseErr SimpleErr where
  store = SimpleErr

data ParsedOpt = ParsedFlag String
               | ParsedSingle String String
               | ParsedDouble String String String
               deriving (Show, Eq)

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
  longs = map makeLong ls
  makeLong s = LongCLItem s no

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
