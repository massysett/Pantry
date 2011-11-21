{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Food where

import qualified Data.Map as M
import Data.Ratio
import Data.Decimal hiding (divide)
import Data.Maybe
import Control.Monad
import Control.Monad.Instances
import Data.List
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Sequence as S
import Types
import Data.Text hiding (filter, map, length, head, null)
import qualified Data.Text as X

newtype Name = Name Text deriving (Eq, Ord, Show)
newtype NutAmt = NutAmt NonNeg deriving (Eq, Ord, Show, Add)
data NutNameAmt = NutNameAmt Name NutAmt deriving Show
newtype NutNamesAmts = NutNamesAmts (M.Map Name NutAmt) deriving Show

newtype NutRatio = NutRatio NonNeg deriving Show
data NutNameRatio = NutNameRatio Name NutRatio deriving Show
newtype NutNamesRatios = NutNamesRatios (M.Map Name NutRatio) deriving Show

newtype Grams = Grams NonNeg deriving (Eq, Ord, Show, Add)
data UnitNameAmt = UnitNameAmt Name Grams deriving Show
newtype UnitNamesAmts = UnitNamesAmts (M.Map Name Grams) deriving Show

newtype TagVal = TagVal Text deriving (Eq, Ord, Show)
data TagNameVal = TagNameVal Name TagVal deriving Show
newtype TagNamesVals = TagNamesVals (M.Map Name TagVal) deriving Show

if' :: Bool -> a -> a -> a
if' b x y = case b of True -> x; False -> y

newtype PctRefuse = PctRefuse BoundedPercent deriving (Eq, Ord, Show)
newtype Qty = Qty (Either NonNeg NonNegMixed) deriving Show
newtype Yield = Yield (Maybe Grams) deriving Show
newtype Ingr = Ingr (S.Seq Food) deriving Show

data Food = Food { tags :: TagNamesVals
                 , units :: UnitNamesAmts
                 , nutRatios :: NutNamesRatios
                 , currUnit :: UnitNameAmt
                 , pctRefuse :: PctRefuse
                 , qty :: Qty
                 , yield :: Yield
                 , ingr :: Ingr
                 , foodId :: Integer } deriving Show

absGrams :: UnitNameAmt
absGrams = UnitNameAmt (Name . pack $ "g") (Grams . partialNewNonNeg $ 1 % 1)

absOunces :: UnitNameAmt
absOunces = UnitNameAmt (Name . pack $ "oz") (Grams . partialNewNonNeg $ 2835 % 1000)

absPounds :: UnitNameAmt
absPounds = UnitNameAmt (Name . pack $ "lb") (Grams . partialNewNonNeg $ 4536 % 10)

emptyFood :: Food
emptyFood = Food { tags = TagNamesVals M.empty
                 , units = UnitNamesAmts M.empty
                 , nutRatios = NutNamesRatios M.empty
                 , currUnit = absGrams
                 , pctRefuse = PctRefuse zeroPercent
                 , qty = Qty (Left zero)
                 , yield = Yield Nothing
                 , ingr = Ingr S.empty
                 , foodId = 0 }

-- Matchers
class Matcher a where
  matches :: a -> Text -> Bool

-- Tag manipulations

hasTag :: Name -> Food -> Bool
hasTag n f = M.member n ts where (TagNamesVals ts) = tags f

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

allAvailUnits :: Food -> UnitNamesAmts
allAvailUnits = allUnits . units

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
foodGrams f = Grams $ q `mult` u where
  (Qty quan) = qty f
  q = case quan of
    (Left nn) -> nn
    (Right mix) -> toNonNeg mix
  (UnitNameAmt _ (Grams u)) = currUnit f

-- Nut manipulations
addNut :: NutNameAmt -> Food -> Either Error Food
addNut (NutNameAmt n a) f = if' notZero (Right newFood) (Left err) where
  g = foodGrams f
  notZero = g /= zero
  newFood = f {nutRatios = newRatios}
  (NutNamesRatios oldRatios) = nutRatios f
  newRatio = NutRatio $ rat `divide` gr where
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
ratioToAmt (Grams g) (NutRatio r) = NutAmt $ g `mult` r

foodIngrNuts :: Food -> NutNamesAmts
foodIngrNuts f = if' ingrZero (NutNamesAmts M.empty) adjusted where
  raw = (\(NutNamesAmts m) -> m) . foldFoodNuts
        . (\(Ingr fs) -> fs) . ingr $ f
  y = fromJust $ recipeYield f
  im = ingredientMass f
  ingrZero = im == (Grams zero)
  (Ingr is) = ingr f
  adjusted = NutNamesAmts $ M.map recipeAdjustedAmt raw
  recipeAdjustedAmt n = NutAmt $ ig `divide` yg `mult` ng where
    (Grams ig) = im
    (Grams yg) = y
    (NutAmt ng) = n

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
  NutNamesAmts $ M.unionWith add l r

foldNuts :: (F.Foldable f) => f NutNamesAmts -> NutNamesAmts
foldNuts = F.foldl' sumNuts (NutNamesAmts M.empty)

foldFoodNuts :: (F.Foldable f, Functor f) => f Food -> NutNamesAmts
foldFoodNuts = foldNuts . fmap foodNuts

-- PctRefuse functions
setPctRefuse :: PctRefuse -> Food -> Food
setPctRefuse p f = f {pctRefuse = p}

minusPctRefuse :: Food -> Food
minusPctRefuse f = f {qty = newQty} where
  newQty = Qty (Left . subtractPercent q $ p)
  (Qty quan) = qty f
  q = case quan of
    (Left nn) -> nn
    (Right mx) -> toNonNeg mx
  (PctRefuse p) = pctRefuse f

-- Ingredient functions

-- | Returns total mass of all ingredients in the food. If there are
-- no ingredients, or if all ingredients have mass of zero, returns
-- zero.
ingredientMass :: Food -> Grams
ingredientMass f = F.foldl' add zero (fmap foodGrams ins) where
  (Ingr ins) = ingr f

-- | Returns the yield - that is, the total mass when one recipe is
-- prepared. If there is a Yield already set for the food, return
-- that. Otherwise, if the food has ingredients and they have positive
-- mass, return that. Otherwise, return Nothing.
recipeYield :: Food -> Maybe Grams
recipeYield f = if' (isJust y) y i where
  (Yield y) = yield f
  i = if' (mR > zero) (Just . Grams $ mR) Nothing
  (Grams mR) = ingredientMass f

-- Ingredient functions

addIngredient :: Food -> Food -> Food
addIngredient i f = f {ingr = Ingr new} where
  (Ingr old) = ingr f
  new = old S.|> i
  
addIngredients :: F.Foldable f =>
                  f Food -> Food -> Food
addIngredients fs f = F.foldl' (flip addIngredient) f fs

addIngredientsToFoods :: (F.Foldable f, Functor u)
                         => f Food -> u Food -> u Food
addIngredientsToFoods fs = fmap (addIngredients fs)

deleteIngredients :: Food -> Food
deleteIngredients f = f {ingr = Ingr S.empty}

data Error = NoMatchingUnit
           | MultipleMatchingUnits [(Name, Grams)]
           | AddNutToZeroQty
           | Other String


