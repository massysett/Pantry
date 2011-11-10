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
                -> String
                -> Either SimpleErr [ParsedOpt]
singleParser n ps s1 = Right $ ps ++ [ParsedSingle s1 s2]

doubleParser :: String
                -> [ParsedOpt]
                -> String
                -> String


