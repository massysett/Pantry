{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types( NonNeg
            , partialNewNonNeg
            , NonNegMixed
            , toNonNeg
            , BoundedPercent
            , subtractPercent
            , HasZero(..)
            , Add(..)
            , Divide(..)
            , FromStr(..) ) where

import Data.Ratio
import Data.Decimal
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Monoid
import qualified Data.Text as X

newtype NonNeg = NonNeg Rational deriving (Eq, Ord, Show)

partialNewNonNeg :: Rational -> NonNeg
partialNewNonNeg r = if r < 0 then e else NonNeg r where
  e = error "partialNewNonNeg: value out of range"

data NonNegMixed = NonNegMixed { mixedDec :: Decimal
                               , mixedRatio :: Rational } deriving Show

instance Eq NonNegMixed where
  l == r = toNonNeg l == toNonNeg r

instance Ord NonNegMixed where
  compare l r = compare (toNonNeg l) (toNonNeg r)

toNonNeg :: NonNegMixed -> NonNeg
toNonNeg (NonNegMixed d r) = NonNeg $ toRational d + r

newtype BoundedPercent = BoundedPercent NonNegMixed
                         deriving (Eq, Ord, Show)

subtractPercent :: NonNeg -> BoundedPercent -> NonNeg
subtractPercent (NonNeg n) (BoundedPercent pct) = nn where
  nn = NonNeg $ n - n * p
  (NonNeg p) = toNonNeg pct

class HasZero a where
  zero :: a

instance HasZero BoundedPercent where
  zero = BoundedPercent zero

instance HasZero NonNegMixed where
  zero = NonNegMixed (Decimal 0 0) (0 % 1)

instance HasZero NonNeg where
  zero = NonNeg $ 0 % 1

class (HasZero a) => Add a where
  add :: a -> a -> a
  mult :: a -> a -> a
  one :: a

instance Add NonNeg where
  add (NonNeg l) (NonNeg r) = NonNeg $ l + r
  mult (NonNeg l) (NonNeg r) = NonNeg $ l * r
  one = NonNeg 1

class Divide a where
  divide :: a -> a -> a

instance Divide NonNeg where
  divide (NonNeg l) (NonNeg r) = NonNeg $ l / r

class FromStr a where
  fromStr :: String -> Maybe a

instance FromStr NonNeg where
  fromStr s = fromStr s >>= return . toNonNeg

instance FromStr NonNegMixed where
  fromStr = either (const Nothing) Just . parse parseNonNegMixed ""

instance FromStr BoundedPercent where
  fromStr s = do
    m <- fromStr s
    let (NonNeg r) = toNonNeg m
        p = r / 100
    when (p < 0 || p >= 1) $ fail "percent out of range"
    return $ BoundedPercent m

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
