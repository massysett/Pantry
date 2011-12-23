{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pantry.Types (
  -- * Numeric types
  -- ** Non-negative
  NonNeg
  , nonNegToRational
  , partialNewNonNeg

  , NonNegInteger
  , unNonNegInteger
  , partialNewNonNegInteger

  , NonNegMixed
  , mixedDec
  , mixedRatio

    -- ** Positive
  , Pos
  , unPos
  , partialNewPos
  , nonNegToPos
    
  , PosInteger
  , partialNewPosInteger
    
  , PosMixed
  , posMixedDec
  , posMixedRatio

  , BoundedPercent
  , pctToMixed
  , subtractPercent

    -- * Typeclasses
  , HasNonNeg(toNonNeg)
  , HasPos(toPos)
  , Next(..)
  , HasZero(..)
  , Add(..)
  , Divide(..)
  , FromStr(..)
  
  -- * Arithmetic
  , nonNegDivPos

  ) where

import Data.Ratio((%), numerator, denominator)
import Pantry.Exact(Exact, exact)
import Data.Decimal(Decimal, DecimalRaw(Decimal))
import Text.ParserCombinators.Parsec(Parser, try, (<|>), char,
                                     eof, digit, many1, parse)
import Control.Monad(when, liftM3)
import Pantry.Rounded(Rounded)
import Data.Serialize(Serialize(put, get))
import Data.Text(snoc, append, pack, Text)

-- | Non negative, rational numbers. Their value can be zero or
-- greater. This is the broadest numeric type.
newtype NonNeg = NonNeg { nonNegToRational :: Rational }
               deriving (Eq, Ord, Show, Exact, Rounded)

instance Serialize NonNeg where
  put (NonNeg r) = put r
  get = do
    r <- get
    return $ NonNeg r

-- | Create a new NonNeg from a Rational. This function is partial. It
-- will crash if its argument is less than zero.
partialNewNonNeg :: Rational -> NonNeg
partialNewNonNeg r = if r < 0 then e else NonNeg r where
  e = error "partialNewNonNeg: value out of range"

-- | Non negative integers. Can be zero or greater. This is not a
-- member of many Prelude typeclasses (such as Enum) because those
-- typeclasses have partial functions.
newtype NonNegInteger = NonNegInteger { unNonNegInteger :: Integer }
                        deriving (Eq, Ord, Show, Exact)

instance Serialize NonNegInteger where
  put (NonNegInteger i) = put i
  get = get >>= return . NonNegInteger

-- | Create a new non negative integer. Partial. Will crash if its
-- argument is less than zero.
partialNewNonNegInteger :: (Integral i) => i -> NonNegInteger
partialNewNonNegInteger i
  | ii < 0 = error "partialNewNonNegInteger: integer is negative."
  | otherwise = NonNegInteger $ ii
    where
      ii = fromIntegral i

-- | Non negative, mixed numbers. Have both a decimal part and a
-- rational part.
data NonNegMixed = NonNegMixed { mixedDec :: Decimal
                               , mixedRatio :: Rational } deriving Show

instance Serialize NonNegMixed where
  put (NonNegMixed (Decimal p m) r) = put p >> put m >> put r
  get = do
    p <- get
    m <- get
    r <- get
    return $ NonNegMixed (Decimal p m) r

instance Exact NonNegMixed where
  exact n = mixedExact (mixedDec n) (mixedRatio n)

mixedExact :: Decimal -> Rational -> Text
mixedExact nd nr = result where
    result = case (d, r) of
      (Just ds, Just rs) -> ds `snoc` ' ' `append` rs
      (Just ds, Nothing) -> ds
      (Nothing, Just rs) -> rs
      (Nothing, Nothing) -> pack "0"
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

-- | Positive rational numbers. Their value must be greater than zero.
newtype Pos = Pos { unPos :: Rational }
              deriving (Show, Serialize, Eq, Ord)

-- | Make a new positive number. This function is partial. It is
-- bottom if its argument is not greater than zero.
partialNewPos :: Rational -> Pos
partialNewPos r = case r > 0 of
  True -> Pos r
  False -> error "partialNewPos: argument not positive"

-- | Convert a NonNeg to a Pos. Fails if the NonNeg is zero.
nonNegToPos :: NonNeg -> Maybe Pos
nonNegToPos (NonNeg nn) = case nn == 0 of
  True -> Nothing
  False -> Just $ Pos nn

-- | Positive integers. Must be greater than zero. Not a member of
-- Prelude typeclasses such as Enum becuase these functions are
-- partial.
newtype PosInteger = PosInteger { unPosInteger :: Integer }
                     deriving (Eq, Ord, Show, Serialize, Exact)

-- | Create a new positive integer. Partial. Will crash if its
-- argument is less than zero.
partialNewPosInteger :: (Integral i) => i -> PosInteger
partialNewPosInteger i
  | ii <= 0 = error "partialNewPosInteger: integer is not positive."
  | otherwise = PosInteger $ ii
    where
      ii = fromIntegral i

-- | Positive mixed numbers. Must be greater than zero.
data PosMixed = PosMixed { posMixedDec :: Decimal
                         , posMixedRatio :: Rational }
                deriving Show

instance Exact PosMixed where
  exact n = mixedExact (posMixedDec n) (posMixedRatio n)

instance Serialize PosMixed where
  put (PosMixed (Decimal p m) r) = put p >> put m >> put r
  get = liftM3 f get get get where
    f p m r = PosMixed (Decimal p m) r

-- | Bounded percent. Represents values between 0 and 100 percent,
-- inclusive. Internally these are held as NonNegMixed values.
newtype BoundedPercent = BoundedPercent { pctToMixed :: NonNegMixed }
                       deriving (Eq, Ord, Show, Exact)

instance Serialize BoundedPercent where
  put (BoundedPercent nnm) = put nnm
  get = get >>= return . BoundedPercent

-- | Reduce a NonNeg by a BoundedPercent.
subtractPercent :: NonNeg -> BoundedPercent -> NonNeg
subtractPercent (NonNeg n) (BoundedPercent pct) = nn where
  nn = NonNeg $ n - n * p / 100
  (NonNeg p) = toNonNeg pct

-- | Things that can be converted to non negative numbers.
class HasNonNeg a where
  toNonNeg :: a -> NonNeg

instance HasNonNeg NonNeg where
  toNonNeg a = a

instance HasNonNeg NonNegInteger where
  toNonNeg (NonNegInteger a) = NonNeg $ toRational a

instance HasNonNeg NonNegMixed where
  toNonNeg (NonNegMixed d r) = NonNeg $ toRational d + r

instance HasNonNeg Pos where
  toNonNeg (Pos a) = NonNeg a

instance HasNonNeg PosInteger where
  toNonNeg (PosInteger a) = NonNeg $ toRational a

instance HasNonNeg PosMixed where
  toNonNeg (PosMixed d r) = NonNeg $ toRational d + r

class HasPos a where
  toPos :: a -> Pos

instance HasPos Pos where
  toPos a = a

instance HasPos PosInteger where
  toPos (PosInteger i) = Pos $ fromIntegral i

instance HasPos PosMixed where
  toPos (PosMixed d r) = Pos ((toRational d) + r)

instance Eq PosMixed where
  (==) l r = toPos l == toPos r

-- | Numbers that can be incremented (e.g. integers, not floats)
class Next a where
  next :: a -> a

instance Next NonNegInteger where
  next = NonNegInteger . succ . unNonNegInteger

instance Next PosInteger where
  next = PosInteger . succ . unPosInteger

-- | Numbers that have a zero value. Non-negative numbers can be
-- instances of this class; numbers that may only be positive cannot
-- be members.
class HasZero a where
  zero :: a

instance HasZero NonNeg where
  zero = NonNeg 0

instance HasZero NonNegInteger where
  zero = NonNegInteger 0

instance HasZero NonNegMixed where
  zero = NonNegMixed (Decimal 0 0) (0 % 1)

instance HasZero BoundedPercent where
  zero = BoundedPercent zero

-- | Numbers that can be added.
class Add a where
  add :: a -> a -> a
  mult :: a -> a -> a

instance Add NonNeg where
  add (NonNeg l) (NonNeg r) = NonNeg $ l + r
  mult (NonNeg l) (NonNeg r) = NonNeg $ l * r

instance Add NonNegInteger where
  add (NonNegInteger l) (NonNegInteger r) = NonNegInteger $ l + r
  mult (NonNegInteger l) (NonNegInteger r) = NonNegInteger $ l * r

instance Add Pos where
  add (Pos l) (Pos r) = Pos $ l + r
  mult (Pos l) (Pos r) = Pos $ l + r

instance Add PosInteger where
  add (PosInteger l) (PosInteger r) = PosInteger $ l + r
  mult (PosInteger l) (PosInteger r) = PosInteger $ l * r

-- | Real division where the result has full precision, but where
-- division might fail because the denominator might be zero.
class Divide a where
  -- | Returns a Nothing for division by zero.
  divide :: a -> a -> Maybe a

instance Divide NonNeg where
  divide (NonNeg l) (NonNeg r)
    | r == 0 = Nothing
    | otherwise = Just . NonNeg $ l / r

instance Divide Pos where
  divide (Pos l) (Pos r) = Just . Pos $ l / r

-- | Things that can be converted from a string.
class FromStr a where
  fromStr :: String -> Maybe a

instance FromStr NonNeg where
  fromStr s = fromStr s >>=
              return . (toNonNeg :: NonNegMixed -> NonNeg)

instance FromStr NonNegMixed where
  fromStr s = case parse parseNonNegMixed "" s of
    (Left _) -> Nothing
    (Right nnm) -> Just nnm

instance FromStr PosMixed where
  fromStr s = do
    nnm <- either (const Nothing) Just (parse parseNonNegMixed "" s)
    case (mixedDec nnm == 0, mixedRatio nnm == 0) of
      (True, True) -> Nothing
      _ -> Just $ PosMixed (mixedDec nnm) (mixedRatio nnm)
      

instance FromStr BoundedPercent where
  fromStr s = do
    m <- fromStr s
    let (NonNeg r) = toNonNeg m
        p = r / 100
    when (p < 0 || p >= 1) $ fail "percent out of range"
    return $ BoundedPercent m

instance FromStr NonNegInteger where
  fromStr = either (const Nothing) (Just . NonNegInteger)
              . parse integer ""

instance FromStr PosInteger where
  fromStr s = case fromStr s of
    (Just (NonNegInteger i)) ->
      case i of 0 -> Nothing
                _ -> Just . PosInteger $ i
    _ -> Nothing

-- | Divide any non-negative by a positive. Always succeeds.
nonNegDivPos :: (HasNonNeg a, HasPos b)
          => a -> b -> NonNeg
nonNegDivPos a b = let (NonNeg ra) = toNonNeg a
                       (Pos rb) = toPos b
                   in NonNeg $ ra / rb

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
