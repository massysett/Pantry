{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Food where

import qualified Data.Map as M
import Data.Ratio
import Data.Decimal
import Text.Regex.PCRE
import Data.Maybe
import Control.Monad
import Control.Monad.Instances

newtype Name = Name String deriving (Eq, Ord, Show)
newtype NutAmt = NutAmt Rational deriving (Eq, Ord, Show, Num, Real)
data NutNameAmt = NutNameAmt Name NutAmt deriving Show
newtype NutNamesAmts = NutNamesAmts (M.Map Name NutAmt) deriving Show

newtype NutRatio = NutRatio Rational deriving (Eq, Ord, Show, Num, Real)
data NutNameRatio = NutNameRatio Name NutRatio deriving Show
newtype NutNamesRatios = NutNamesRatios (M.Map Name NutRatio) deriving Show

newtype Grams = Grams Rational deriving (Eq, Ord, Show, Num, Real)
data UnitNameAmt = UnitNameAmt Name Grams deriving Show
newtype UnitNamesAmts = UnitNamesAmts (M.Map Name Grams) deriving Show

newtype TagVal = TagVal String deriving (Eq, Ord, Show)
data TagNameVal = TagNameVal Name TagVal deriving Show
newtype TagNamesVals = TagNamesVals (M.Map Name TagVal) deriving Show

if' :: Bool -> a -> a -> a
if' b x y = case b of True -> x; False -> y

data MixedNum = MixedNum Decimal Rational deriving (Eq, Show)
instance Num MixedNum where
  (+) (MixedNum d1 r1) (MixedNum d2 r2) =
    MixedNum (d1 + d2) (r1 + r2)
  (-) (MixedNum d1 r1) (MixedNum d2 r2) =
    MixedNum (d1 - d2) (r1 - r2)
  (*) (MixedNum d1 r1) (MixedNum d2 r2) =
    MixedNum (d1 * d2) (r1 * r2)
  abs (MixedNum d r) = MixedNum (abs d) (abs r)
  signum (MixedNum d r) = if' (s == 0) zero signed where
    zero = MixedNum (Decimal 0 0) (0 % 1)
    signed = if' (s < 0) negative positive
    negative = MixedNum (Decimal 0 (-1)) (0 % 1)
    positive = MixedNum (Decimal 0 1) (0 % 1)
    s = toRational d + r
  fromInteger i = MixedNum (fromInteger i) (0 % 1)

instance Ord MixedNum where
  (<=) (MixedNum d1 r1) (MixedNum d2 r2) =
    (toRational d1 + r1) <= (toRational d2 + r2)

instance Real MixedNum where
  toRational (MixedNum d r) = toRational d + r

newtype PctRefuse = PctRefuse Decimal deriving Show
newtype Qty = Qty MixedNum deriving (Eq, Ord, Num, Real, Show)
newtype Yield = Yield (Maybe Decimal) deriving Show
newtype Ingr = Ingr [Food] deriving Show

data Food = Food { tags :: TagNamesVals
                 , units :: UnitNamesAmts
                 , nutRatios :: NutNamesRatios
                 , currUnit :: UnitNameAmt
                 , pctRefuse :: PctRefuse
                 , qty :: Qty
                 , yield :: Yield
                 , ingr :: Ingr } deriving Show

absGrams :: UnitNameAmt
absGrams = UnitNameAmt (Name "g") (Grams $ 1 % 1)

absOunces :: UnitNameAmt
absOunces = UnitNameAmt (Name "oz") (Grams $ 2835 % 1000)

absPounds :: UnitNameAmt
absPounds = UnitNameAmt (Name "lb") (Grams $ 4536 % 10)

emptyFood :: Food
emptyFood = Food { tags = TagNamesVals M.empty
                 , units = UnitNamesAmts M.empty
                 , nutRatios = NutNamesRatios M.empty
                 , currUnit = absGrams
                 , pctRefuse = PctRefuse (Decimal 0 0)
                 , qty = Qty (MixedNum (Decimal 0 100) (0 % 1))
                 , yield = Yield Nothing
                 , ingr = Ingr [] }

-- Matchers
class Matcher a where
  matches :: a -> String -> Bool

-- Tag manipulations

getTag :: Name -> Food -> Maybe TagNameVal
getTag n f = do
  let (TagNamesVals m) = tags f
  v <- M.lookup n m
  return $ TagNameVal n v

addTag :: TagNameVal -> Food -> Food
addTag (TagNameVal n (TagVal v)) f = f {tags = new} where
  (TagNamesVals old) = tags f
  new = TagNamesVals (M.insert n (TagVal v) old)

-- | Delete tags whose name matches specified pattern.
deleteTag :: Matcher m => m -> Food -> Food
deleteTag m f = f {tags = new} where
  (TagNamesVals old) = tags f
  new = TagNamesVals $ M.fromList ns
  ns = filter (not . matches m . tagName . fst) (M.assocs old)
  tagName n = let (Name v) = n in v

-- | True if food has a tag whose name matches the Name and whose
-- value matches the second matcher.
foodMatch :: Matcher m => Name -> m -> Food -> Bool
foodMatch n v f = isJust $ do
  (TagNameVal _ (TagVal val)) <- getTag n f
  guard $ matches v val

-- Units manipulations
-- |Add an arbitrary unit to a food.
addUnit :: UnitNameAmt -> Food -> Food
addUnit (UnitNameAmt n a) f = f {units = new} where
  (UnitNamesAmts old) = units f
  new = UnitNamesAmts $ M.insert n a old

-- | Delete arbitrary units whose name matches a matcher.
deleteUnits :: Matcher m => m -> Food -> Food
deleteUnits m f = f {units = new} where
  (UnitNamesAmts old) = units f
  new = UnitNamesAmts $ M.fromList ns
  ns = filter (not . matches m . unitName . fst) (M.assocs old)
  unitName n = let (Name v) = n in v

allUnits :: UnitNamesAmts -> UnitNamesAmts
allUnits (UnitNamesAmts m) = UnitNamesAmts $ M.fromList new where
  new = ars ++ abs
  ars = M.assocs m
  abs = map (\(UnitNameAmt n a) -> (n, a))
        [absGrams, absOunces, absPounds]

-- | Change current unit to the one matching a matcher.
changeCurrUnit :: (Matcher m) => m -> Food -> Either Error Food
changeCurrUnit m f = if' oneMatch (Right updateFood) (Left err) where
  oneMatch = length matches == 1
  allU = allUnits $ units f
  matches = filter pred $ M.assocs allU
  pred ((Name n), _) = matches m n
  updateFood = f {currUnit = head matches}
  err = if' (null matches) NoMatchingUnit (MultipleMatchingUnits matches)

{-
changeCurrUnit :: String -> Food -> Either Error Food
changeCurrUnit p f = if' oneMatch (Right updateFood) (Left err) where
  oneMatch = length matches == 1
  allU = allUnitsMap $ units f
  matches = filter (=~ p) (M.keys allU)
  newUnitWrapped = allU M.! head matches
  newUnit = case newUnitWrapped of
    (Left abs) -> Absolute abs
    (Right arb) -> Arbitrary arb
  updateFood = f {currUnit = newUnit}
  err = if null matches
        then NoMatchingUnit
        else MultipleMatchingUnits matches

-- Nuts manipulations
nutRatioToNutAmt :: Grams -> NutRatio -> NutAmt
nutRatioToNutAmt (Grams g) (NutRatio r) = NutAmt $ g * r

nut

absUnitGrams :: AbsUnit -> Grams
absUnitGrams AbsGrams = Grams $ 1 % 1
absUnitGrams Ounces = Grams $ 2835 % 100
absUnitGrams Pounds = Grams $ 4536 % 10

foodGrams :: Food -> Grams
foodGrams f = Grams $ toRational (qty f) * toRational grams where
  grams = case (currUnit f) of
    (Absolute abs) -> absUnitGrams abs
    (Arbitrary (Unit _ g)) -> g


addNut :: Food -> Nut -> Either Error Food
addNut f (Nut n v) = f {nuts = newNuts} where
-}

data Error = NoMatchingUnit
           | MultipleMatchingUnits [UnitNameAmt]
           | AddNutToZeroQty
           | Other String


