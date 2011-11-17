{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Food where

import qualified Data.Map as M
import Data.Ratio
import Data.Decimal
import Data.Maybe
import Control.Monad
import Control.Monad.Instances
import Data.List
import qualified Data.Foldable as F
import qualified Data.Traversable as T

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

newtype PctRefuse = PctRefuse MixedNum deriving Show
newtype Qty = Qty MixedNum deriving (Eq, Ord, Num, Real, Show)
newtype Yield = Yield (Maybe MixedNum) deriving Show
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
                 , pctRefuse = PctRefuse (MixedNum (Decimal 0 0) (0 % 1))
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

changeTag :: TagNameVal -> Food -> Food
changeTag (TagNameVal n (TagVal v)) f = f {tags = new} where
  (TagNamesVals old) = tags f
  new = TagNamesVals (M.insert n (TagVal v) old)

changeTags :: F.Foldable f => f TagNameVal -> Food -> Food
changeTags ts f = F.foldl' (flip changeTag) f ts

changeTagsInFoods :: (F.Foldable a, Functor b) =>
                     a TagNameVal
                     -> b Food
                     -> b Food
changeTagsInFoods ts = fmap (changeTags ts)

deleteTag :: (Matcher m) => m -> Food -> Food
deleteTag m f = f { tags = new } where
  (TagNamesVals old) = tags f
  new = TagNamesVals . M.fromList . filter p . M.assocs $ old
  p (Name n, _) = not $ matches m n

deleteTags :: (Matcher m, F.Foldable f) => f m -> Food -> Food
deleteTags ms f = F.foldl' (flip deleteTag) f ms

deleteTagsInFoods :: (Matcher m, F.Foldable a, Functor b)
                     => a m
                     -> b Food
                     -> b Food
deleteTagsInFoods ms fs = fmap (deleteTags ms) fs

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

addUnits :: F.Foldable f => f UnitNameAmt -> Food -> Food
addUnits us f = F.foldl' (flip addUnit) f us

addUnitsToFoods :: (F.Foldable f, Functor l)
                   => f UnitNameAmt
                   -> l Food
                   -> l Food
addUnitsToFoods us = fmap (addUnits us)

-- | Delete arbitrary units whose name matches a matcher.
deleteUnit :: Matcher m => m -> Food -> Food
deleteUnit m f = f {units = new} where
  (UnitNamesAmts old) = units f
  new = UnitNamesAmts $ M.fromList ns
  ns = filter (not . matches m . unitName . fst) (M.assocs old)
  unitName n = let (Name v) = n in v

deleteUnits :: (Matcher m, F.Foldable f)
               => f m
               -> Food
               -> Food
deleteUnits ms f = F.foldl' (flip deleteUnit) f ms

deleteUnitsFromFoods :: (Matcher m, F.Foldable a, Functor b)
                        => a m -> b Food -> b Food
deleteUnitsFromFoods ms = fmap (deleteUnits ms)

allUnits :: UnitNamesAmts -> UnitNamesAmts
allUnits (UnitNamesAmts m) = UnitNamesAmts $ M.fromList new where
  new = ars ++ abs
  ars = M.assocs m
  abs = map (\(UnitNameAmt n a) -> (n, a))
        [absGrams, absOunces, absPounds]

-- | Change current unit to the one matching a matcher.
changeCurrUnit :: (Matcher m) => m -> Food -> Either Error Food
changeCurrUnit m f = if' oneMatch (Right newFood) (Left err) where
  oneMatch = length ms == 1
  (UnitNamesAmts allU) = allUnits $ units f
  newFood = f {currUnit = newUnit}
  newUnit = UnitNameAmt headMatchName headMatchGrams
  headMatchName = fst . head $ ms
  headMatchGrams = snd . head $ ms
  ms = filter pred $ M.assocs allU
  pred ((Name n), _) = matches m n
  err = if' (null ms) NoMatchingUnit (MultipleMatchingUnits ms)

changeCurrUnits :: (Matcher m, T.Traversable t)
                   => m -> t Food -> Either Error (t Food)
changeCurrUnits m = T.mapM (changeCurrUnit m)

foodGrams :: Food -> Grams
foodGrams f = Grams $ q * u where
  (Qty quan) = qty f
  q = toRational quan
  (UnitNameAmt _ (Grams unit)) = currUnit f
  u = toRational unit

-- Nut manipulations
addNut :: NutNameAmt -> Food -> Either Error Food
addNut (NutNameAmt n a) f = if' notZero (Right newFood) (Left err) where
  g = foodGrams f
  notZero = g /= (Grams 0)
  newFood = f {nutRatios = newRatios}
  (NutNamesRatios oldRatios) = nutRatios f
  newRatio = NutRatio $ rat / gr where
    (NutAmt rat) = a
    (Grams gr) = g
  newRatios = NutNamesRatios $ M.insert n newRatio oldRatios
  err = AddNutToZeroQty

addNuts :: (F.Foldable f) => f NutNameAmt -> Food -> Either Error Food
addNuts ns f = F.foldlM (flip addNut) f ns

addNutsToFoods :: (F.Foldable a, T.Traversable t)
                  => a NutNameAmt
                  -> t Food
                  -> Either Error (t Food)
addNutsToFoods ns = T.mapM (addNuts ns)

ratioToAmt :: Grams -> NutRatio -> NutAmt
ratioToAmt (Grams g) (NutRatio r) = NutAmt $ g * r

foodIngrNuts :: Food -> NutNamesAmts
foodIngrNuts = foldFoodNuts . (\(Ingr fs) -> fs) . ingr

foodNuts :: Food -> NutNamesAmts
foodNuts f = NutNamesAmts new where
  new = M.union abs ing
  (NutNamesAmts abs) = foodAbsNuts f
  (NutNamesAmts ing) = foodIngrNuts f

foodAbsNuts :: Food -> NutNamesAmts
foodAbsNuts f = NutNamesAmts $ M.map (ratioToAmt g) old where
  g = foodGrams f
  (NutNamesRatios old) = nutRatios f

sumNuts :: NutNamesAmts -> NutNamesAmts -> NutNamesAmts
sumNuts (NutNamesAmts l) (NutNamesAmts r) =
  NutNamesAmts $ M.unionWith (+) l r

foldNuts :: (F.Foldable f) => f NutNamesAmts -> NutNamesAmts
foldNuts = F.foldl' sumNuts (NutNamesAmts M.empty)

foldFoodNuts :: (F.Foldable f, Functor f) => f Food -> NutNamesAmts
foldFoodNuts = foldNuts . fmap foodNuts

setPctRefuse :: PctRefuse -> Food -> Food
setPctRefuse p f = f {pctRefuse = p}

minusPctRefuse :: Food -> Food
minusPctRefuse f = f {qty = newQty} where
  newQty = Qty (MixedNum 0 $ old - old * (pr / 100))
  pr = toRational $ (\(PctRefuse d) -> d) (pctRefuse f)
  old = toRational . qty $ f

recipeYield :: Food -> Maybe Grams
recipeYield f = if' (null ins) Nothing (Just g) where
  (Ingr ins) = ingr f
  g = if' (isNothing y) ingrSum (Grams $ toRational (fromJust y))
  (Yield y) = yield f
  ingrSum = sum . map foodGrams $ ins

recipeAdjustedAmt :: Grams -- ^ Recipe yield
                     -> Grams -- ^ Mass of all ingredients
                     -> NutAmt
                     -> NutAmt
recipeAdjustedAmt y a n = NutAmt $ ag / yg * ng where
  (Grams ag) = a
  (Grams yg) = y
  (NutAmt ng) = n

data Error = NoMatchingUnit
           | MultipleMatchingUnits [(Name, Grams)]
           | AddNutToZeroQty
           | Other String


