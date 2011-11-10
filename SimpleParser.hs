module SimpleParser where

import OptParse

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
