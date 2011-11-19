{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types( NonNeg
            , NonNegMixed
            , Add(..)
            , toNonNeg
            , strToNonNeg
            , strToNonNegMixed ) where

import Data.Ratio
import Data.Decimal
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Monoid

newtype NonNeg = NonNeg { unNonNeg :: Rational } deriving (Eq, Ord, Show)

data NonNegMixed = NonNegMixed { mixedDec :: Decimal
                               , mixedRatio :: Rational } deriving Show

instance Eq NonNegMixed where
  l == r = toNonNeg l == toNonNeg r

instance Ord NonNegMixed where
  compare l r = compare (toNonNeg l) (toNonNeg r)

class Add a where
  zero :: a
  add :: a -> a -> a
  mult :: a -> a -> a
  one :: a

instance Add NonNeg where
  zero = NonNeg 0
  add (NonNeg l) (NonNeg r) = NonNeg $ l + r
  mult (NonNeg l) (NonNeg r) = NonNeg $ l * r
  one = NonNeg 1

strToNonNeg :: String -> Maybe NonNeg
strToNonNeg s = strToNonNegMixed s >>= return . toNonNeg

toNonNeg :: NonNegMixed -> NonNeg
toNonNeg (NonNegMixed d r) = NonNeg $ toRational d + r

strToNonNegMixed :: String -> Maybe NonNegMixed
strToNonNegMixed = either (const Nothing) Just . parse parseNonNegMixed ""

parseNonNegMixed :: Parser NonNegMixed
parseNonNegMixed = try frac <|> try decimalFraction <|> dec where
  frac = do
    f <- fractionAlone
    return $ NonNegMixed (Decimal 0 0) f
  dec = do
    d <- decimalAlone
    return $ NonNegMixed d (0 % 1)

fraction :: Parser Rational
fraction = do
  n <- integer
  _ <- char ' ' <|> return ' '
  _ <- char '/'
  _ <- char ' ' <|> return ' '
  d <- integer
  when (d == 0) $ fail "fraction with zero denominator"
  return (n % d)

decimalFraction :: Parser NonNegMixed
decimalFraction = do
  d <- try decimal <|> integerDec
  _ <- char ' '
  f <- fraction
  eof
  return $ NonNegMixed d f

fractionAlone :: Parser Rational
fractionAlone = do
  f <- fraction
  eof
  return f

decimalAlone :: Parser Decimal
decimalAlone = do
  d <- try decimal <|> integerDec
  eof
  return d
  
integerDec :: Parser Decimal
integerDec = do
  i <- integer
  return . fromInteger $ i

integer :: Parser Integer
integer = do
  d <- many1 digit
  return $ read d

decimal :: Parser Decimal
decimal = do
  s <- decimalStr
  return $ read s

decimalStr :: Parser String
decimalStr = do
  whole <- many1 digit
  _ <- char '.'
  frac <- many1 digit
  return $ whole ++ "." ++ frac
