{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pantry.Food where

import qualified Data.Map as M
import Data.Ratio
import Data.Maybe
import Control.Monad
import Data.List
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Pantry.Types
import Data.Text(Text, pack)
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Pantry.Exact(Exact(exact))
import Pantry.Rounded(Rounded)
import Data.Serialize (Serialize(put, get))
import Data.Monoid as Monoid
import Pantry.Error as R

type Matcher = (Text -> Bool)
type Xform = (Food -> Either R.Error Food)

-- * Data types within Foods

-- | Amount of a nutrient
newtype NutAmt = NutAmt { unNutAmt :: NonNeg }
                 deriving (Eq, Ord, Show, Add, HasZero,
                           Exact, Rounded, Serialize)

-- | Nutrient name and amount paired together. This is stored in a
-- food to indicate its current unit.
data NutNameAmt = NutNameAmt { nutName :: Name 
                             , nutAmt :: NutAmt } deriving Show
instance HasName NutNameAmt where name = nutName

instance Serialize NutNameAmt where
  put (NutNameAmt n a) = put n >> put a
  get = do n <- get; a <- get; return $ NutNameAmt n a

-- | Map of nutrient names and amounts. This is NOT stored in a food;
-- see NutsPerG for that.
newtype NutNamesAmts =
  NutNamesAmts { unNutNamesAmts :: M.Map Name NutAmt }
  deriving (Show, Serialize)

-- | Nutrients per gram of food
newtype NutsPerG = NutsPerG { unNutsPerG :: NonNeg }
                   deriving (Show, Exact, Rounded, Serialize)

-- | Map of nutrient names and the nutrient amount per gram of
-- food. This is stored inside a food.
newtype NutNamesPerGs =
  NutNamesPerGs { unNutNamesPerGs :: (M.Map Name NutsPerG) }
  deriving (Show, Serialize)

-- | Grams expressed as a mixed number.
newtype MixedGrams = MixedGrams { unMixedGrams :: NonNegMixed }
                     deriving (Show, Exact, Serialize)

-- | A unit's name paired with its amount.
data UnitNameAmt = UnitNameAmt { unitName :: Name
                               , unitGrams :: Grams } deriving Show

instance Serialize UnitNameAmt where
  put (UnitNameAmt n a) = put n >> put a
  get = do n <- get; a <- get; return $ UnitNameAmt n a

instance HasName UnitNameAmt where name = unitName

-- | Map of unit names and their corresponding amounts.
newtype UnitNamesAmts =
  UnitNamesAmts { unUnitNamesAmts :: (M.Map Name Grams) }
  deriving (Show, Serialize)

-- | The value of a tag.
newtype TagVal = TagVal { unTagVal :: Text }
               deriving (Eq, Ord, Show, Exact)
instance Serialize TagVal where
  put (TagVal t) = put . encodeUtf8 $ t
  get = get >>= return . TagVal . decodeUtf8

data TagNameVal = TagNameVal { tagName :: Name
                             , tagVal :: TagVal }
                deriving Show
instance Serialize TagNameVal where
  put (TagNameVal n v) = put n >> put v
  get = do n <- get; v <- get; return $ TagNameVal n v

instance HasName TagNameVal where name = tagName

newtype TagNamesVals =
  TagNamesVals { unTagNamesVals :: (M.Map Name TagVal) }
  deriving (Show, Serialize)

-- | The percentage of refuse in a food.
newtype PctRefuse = PctRefuse {unPctRefuse :: BoundedPercent }
                    deriving (Eq, Ord, Show, HasZero, Exact, Serialize)

-- | A food's quantity. If this was human input, it will be a
-- NonNegMixed. If it was computed, it will be a NonNeg.
newtype Qty = Qty { unQty :: (Either NonNeg NonNegMixed) }
            deriving (Show, Serialize)

instance Exact Qty where
  exact (Qty q) = either exact exact q

-- | A food's recipe yield. If this was input by the user, it will be Just
-- MixedGrams. If not input, or if explicitly unset, it will be
-- Nothing; then Pantry will compute the yield.
newtype Yield = Yield { unYield :: (Maybe MixedGrams) }
              deriving (Show, Serialize)

-- | Ingredients
newtype Ingr = Ingr { unIngr :: [Food] }
             deriving (Show, Serialize, Monoid)

-- * Other datatypes

-- | NutRatio is not within the Food datatype, but reports use it. For
-- now this seems to be the best module to put this in.
newtype NutRatio = NutRatio NonNeg
                   deriving (Show, Exact, Rounded, Serialize)

nutRatio :: NutAmt -> NutAmt -> Maybe NutRatio
nutRatio (NutAmt x) (NutAmt y) = do
  q <- divide x y
  return $ NutRatio q

-- * The Food datatype

-- | Represents all foods, both recipes (which have ingredients) and
-- non-recipes (which do not have ingredients). If you can build a
-- Food, it is a valid food, so this datatype is exported
-- program-wide.
data Food = Food { tags :: TagNamesVals
                 , units :: UnitNamesAmts
                 , nutsPerGs :: NutNamesPerGs
                 , currUnit :: UnitNameAmt
                 , pctRefuse :: PctRefuse
                 , qty :: Qty
                 , yield :: Yield
                 , ingr :: Ingr
                 , foodId :: FoodId } deriving Show

instance Serialize Food where
  put f = put (tags f)
          >> put (units f)
          >> put (nutsPerGs f)
          >> put (currUnit f)
          >> put (pctRefuse f)
          >> put (qty f)
          >> put (yield f)
          >> put (ingr f)
          >> put (foodId f)
  get = do
    gtags <- get
    gunits <- get
    gnutsPerGs <- get
    gcurrUnit <- get
    gpctRefuse <- get
    gqty <- get
    gyield <- get
    gingr <- get
    gfoodId <- get
    return Food { tags = gtags
                , units = gunits
                , nutsPerGs = gnutsPerGs
                , currUnit = gcurrUnit
                , pctRefuse = gpctRefuse
                , qty = gqty
                , yield = gyield
                , ingr = gingr
                , foodId = gfoodId }

absGrams :: UnitNameAmt
absGrams = UnitNameAmt (Name . pack $ "grams") (Grams . partialNewNonNeg $ 1 % 1)

absOunces :: UnitNameAmt
absOunces = UnitNameAmt (Name . pack $ "oz") (Grams . partialNewNonNeg $ 2835 % 1000)

absPounds :: UnitNameAmt
absPounds = UnitNameAmt (Name . pack $ "lb") (Grams . partialNewNonNeg $ 4536 % 10)

emptyFood :: Food
emptyFood = Food { tags = TagNamesVals M.empty
                 , units = UnitNamesAmts M.empty
                 , nutsPerGs = NutNamesPerGs M.empty
                 , currUnit = absGrams
                 , pctRefuse = zero
                 , qty = Qty (Left zero)
                 , yield = Yield Nothing
                 , ingr = Ingr []
                 , foodId = zeroFoodId }


-- | Delete all items from a map whose names match a matcher.
deleteMapKeys :: (Text -> Bool) -> M.Map Name v -> M.Map Name v
deleteMapKeys p = M.fromList . filter p' . M.assocs where
  p' (Name n, _) = p n

-- Tag manipulations

hasTag :: Name -> Food -> Bool
hasTag n f = M.member n ts where (TagNamesVals ts) = tags f

getTag :: Name -> Food -> Maybe TagNameVal
getTag n f = do
  let (TagNamesVals m) = tags f
  v <- M.lookup n m
  return $ TagNameVal n v

tagPred :: Name -> Matcher -> Food -> Bool
tagPred n m f = case getTag n f of
  Nothing -> False
  (Just (TagNameVal _ (TagVal x))) -> m x

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

deleteTag :: Name -> Food -> Food
deleteTag t f = f { tags = new } where
  (TagNamesVals old) = tags f
  new = TagNamesVals $ M.delete t old

deleteTagByPat :: (Text -> Bool) -> Food -> Food
deleteTagByPat m f = f { tags = new } where
  (TagNamesVals old) = tags f
  new = TagNamesVals . M.fromList . filter p . M.assocs $ old
  p (Name n, _) = not $ m n

deleteTagsByPat :: (F.Foldable f)
              => f (Text -> Bool) -> Food -> Food
deleteTagsByPat ms f = F.foldl' (flip deleteTagByPat) f ms

deleteTagsInFoodsByPat :: (F.Foldable a, Functor b)
                     => a (Text -> Bool)
                     -> b Food
                     -> b Food
deleteTagsInFoodsByPat ms fs = fmap (deleteTagsByPat ms) fs

changeQty :: Qty -> Food -> Food
changeQty q f = f { qty = q }

-- | True if food has a tag whose name matches the Name and whose
-- value matches the second matcher.
foodMatch :: Name -> (Text -> Bool) -> Food -> Bool
foodMatch n v f = isJust $ do
  (TagNameVal _ (TagVal val)) <- getTag n f
  guard $ v val

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
deleteUnit :: (Text -> Bool) -> Food -> Food
deleteUnit m f = f {units = new} where
  (UnitNamesAmts old) = units f
  new = UnitNamesAmts $ M.fromList ns
  ns = filter (not . m . uN . fst) (M.assocs old)
  uN n = let (Name v) = n in v

deleteUnits :: (F.Foldable f)
               => f (Text -> Bool)
               -> Food
               -> Food
deleteUnits ms f = F.foldl' (flip deleteUnit) f ms

deleteUnitsFromFoods :: (F.Foldable a, Functor b)
                        => a (Text -> Bool) -> b Food -> b Food
deleteUnitsFromFoods ms = fmap (deleteUnits ms)

allAvailUnits :: Food -> UnitNamesAmts
allAvailUnits = allUnits . units

allUnits :: UnitNamesAmts -> UnitNamesAmts
allUnits (UnitNamesAmts m) = UnitNamesAmts $ M.fromList new where
  new = ars ++ absU
  ars = M.assocs m
  absU = map (\(UnitNameAmt n a) -> (n, a))
         [absGrams, absOunces, absPounds]

-- | Change current unit to the one matching a matcher.
changeCurrUnit :: (Text -> Bool) -> Food -> Either R.Error Food
changeCurrUnit m f = if' oneMatch (Right newFood) (Left err) where
  oneMatch = length ms == 1
  (UnitNamesAmts allU) = allUnits $ units f
  newFood = f {currUnit = newUnit}
  newUnit = UnitNameAmt headMatchName headMatchGrams
  headMatchName = fst . head $ ms
  headMatchGrams = snd . head $ ms
  ms = filter prev $ M.assocs allU
  prev ((Name n), _) = m n
  err = if' (null ms) R.NoMatchingUnit (R.MultipleMatchingUnits ms)

changeCurrUnits :: (T.Traversable t)
                   => (Text -> Bool) -> t Food -> Either R.Error (t Food)
changeCurrUnits m = T.mapM (changeCurrUnit m)

foodGrams :: Food -> Grams
foodGrams f = Grams $ q `mult` u where
  (Qty quan) = qty f
  q = case quan of
    (Left nn) -> nn
    (Right mix) -> toNonNeg mix
  (UnitNameAmt _ (Grams u)) = currUnit f

-- Nut manipulations
addNut :: NutNameAmt -> Food -> Either R.Error Food
addNut (NutNameAmt n a) f = if' notZero (Right newFood) (Left err) where
  g = foodGrams f
  notZero = g /= zero
  newFood = f {nutsPerGs = newPerGs}
  (NutNamesPerGs oldPerGs) = nutsPerGs f
  newPerG = NutsPerG . fromJust $ rat `divide` gr where
    (NutAmt rat) = a
    (Grams gr) = g
  newPerGs = NutNamesPerGs $ M.insert n newPerG oldPerGs
  err = R.AddNutToZeroQty

addNuts :: (F.Foldable f) => f NutNameAmt -> Food -> Either R.Error Food
addNuts ns f = F.foldlM (flip addNut) f ns

addNutsToFoods :: (F.Foldable a, T.Traversable t)
                  => a NutNameAmt
                  -> t Food
                  -> Either R.Error (t Food)
addNutsToFoods ns = T.mapM (addNuts ns)

nutsPerGToAmt :: Grams -> NutsPerG -> NutAmt
nutsPerGToAmt (Grams g) (NutsPerG r) = NutAmt $ g `mult` r

foodIngrNuts :: Food -> NutNamesAmts
foodIngrNuts f = if' ingrZero (NutNamesAmts M.empty) adjusted where
  raw = (\(NutNamesAmts m) -> m) . foldFoodNuts
        . (\(Ingr fs) -> fs) . ingr $ f
  y = fromJust $ recipeYield f
  im = ingredientMass f
  ingrZero = im == (Grams zero)
  adjusted = NutNamesAmts $ M.map recipeAdjustedAmt raw
  recipeAdjustedAmt n = NutAmt $ qu `mult` ng where
    qu = fromJust $ ig `divide` yg
    (Grams ig) = im
    (Grams yg) = y
    (NutAmt ng) = n

foodNuts :: Food -> NutNamesAmts
foodNuts f = NutNamesAmts new where
  new = M.union absU ing
  (NutNamesAmts absU) = foodAbsNuts f
  (NutNamesAmts ing) = foodIngrNuts f

getNut :: Name -> Food -> Maybe NutAmt
getNut n = M.lookup n
           . (\(NutNamesAmts m) -> m)
           . foodNuts

foodAbsNuts :: Food -> NutNamesAmts
foodAbsNuts f = NutNamesAmts $ M.map (nutsPerGToAmt g) old where
  g = foodGrams f
  (NutNamesPerGs old) = nutsPerGs f

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
recipeYield f = if' (isJust y) gr i where
  gr = Just . Grams . toNonNeg . (\(MixedGrams m) -> m) . fromJust $ y
  (Yield y) = yield f
  i = if' (mR > zero) (Just . Grams $ mR) Nothing
  (Grams mR) = ingredientMass f

-- Ingredient functions

addIngredient :: Food -> Food -> Food
addIngredient i f = f {ingr = Ingr new} where
  (Ingr old) = ingr f
  new = old ++ [i]
  
addIngredients :: F.Foldable f =>
                  f Food -> Food -> Food
addIngredients fs f = F.foldl' (flip addIngredient) f fs

addIngredientsToFoods :: (F.Foldable f, Functor u)
                         => f Food -> u Food -> u Food
addIngredientsToFoods fs = fmap (addIngredients fs)

deleteIngredients :: Food -> Food
deleteIngredients f = f {ingr = Ingr [] }

if' :: Bool -> a -> a -> a
if' b x y = case b of True -> x; False -> y
