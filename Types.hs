{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types( NonNeg
            , nonNegToRational
            , partialNewNonNeg
            , NonNegMixed
            , mixedDec
            , mixedRatio
            , toNonNeg
            , NonNegInteger
            , PosInteger
            , BoundedPercent
            , pctToMixed
            , subtractPercent
            , Next(..)
            , HasZero(..)
            , Add(..)
            , Divide(..)
            , FromStr(..) ) where

import Prelude(Eq, Ord, Show, (==), (-), ($), (+), compare,
               (<), error, toRational, (*), (/),
               Maybe(Nothing, Just), (.), either, const,
               (||), (>=), fromInteger, read, show, String,
               (++), otherwise, Integer, succ)
import Data.Ratio(Rational, (%), numerator, denominator)
import Exact(Exact, exact)
import Data.Decimal(Decimal, DecimalRaw(Decimal))
import Text.ParserCombinators.Parsec(Parser, try, (<|>), char,
                                     eof, digit, many1, parse)
import Control.Monad((>>=), return, when, fail)
import Data.Text(snoc, append, pack)
import Rounded(Rounded)

newtype NonNeg = NonNeg { nonNegToRational :: Rational }
               deriving (Eq, Ord, Show, Exact, Rounded)

partialNewNonNeg :: Rational -> NonNeg
partialNewNonNeg r = if r < 0 then e else NonNeg r where
  e = error "partialNewNonNeg: value out of range"

data NonNegMixed = NonNegMixed { mixedDec :: Decimal
                               , mixedRatio :: Rational } deriving Show

instance Exact NonNegMixed where
  exact n = result where
    result = case (d, r) of
      (Just ds, Just rs) -> ds `snoc` ' ' `append` rs
      (Just ds, Nothing) -> ds
      (Nothing, Just rs) -> rs
      (Nothing, Nothing) -> pack "0"
    nd = mixedDec n
    nr = mixedRatio n
    d | nd == Decimal 0 0 = Nothing
      | otherwise = Just . pack . show $ nd
    r | numerator nr == 0 = Nothing
      | denominator nr == 1 = Just . pack $ num
      | otherwise = Just . pack $ num ++ "/" ++ den where
        num = show . numerator $ nr
        den = show . denominator $ nr

instance Eq NonNegMixed where
  l == r = toNonNeg l == toNonNeg r

instance Ord NonNegMixed where
  compare l r = compare (toNonNeg l) (toNonNeg r)

toNonNeg :: NonNegMixed -> NonNeg
toNonNeg (NonNegMixed d r) = NonNeg $ toRational d + r

newtype NonNegInteger = NonNegInteger { unNonNegInteger :: Integer }
                        deriving (Eq, Ord, Show)

newtype PosInteger = PosInteger { unPosInteger :: Integer }
                     deriving (Eq, Ord, Show)

newtype BoundedPercent = BoundedPercent { pctToMixed :: NonNegMixed }
                       deriving (Eq, Ord, Show, Exact)

subtractPercent :: NonNeg -> BoundedPercent -> NonNeg
subtractPercent (NonNeg n) (BoundedPercent pct) = nn where
  nn = NonNeg $ n - n * p / 100
  (NonNeg p) = toNonNeg pct

class Next a where
  next :: a -> a

instance Next NonNegInteger where
  next = NonNegInteger . succ . unNonNegInteger

instance Next PosInteger where
  next = PosInteger . succ . unPosInteger

class HasZero a where
  zero :: a

instance HasZero BoundedPercent where
  zero = BoundedPercent zero
instance HasZero NonNegMixed where
  zero = NonNegMixed (Decimal 0 0) (0 % 1)
instance HasZero NonNegInteger where
  zero = NonNegInteger 0

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
  divide :: a -> a -> Maybe a

instance Divide NonNeg where
  divide (NonNeg l) (NonNeg r)
    | r == 0 = Nothing
    | otherwise = Just . NonNeg $ l / r

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

-- Parsec basement

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
