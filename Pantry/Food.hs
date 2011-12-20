{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module containts the Food data type, along with types that
-- are within the Food type. There are some simple functions in here
-- to manipulate Food values; to the extent that such functions can
-- fail, they do not return anything that is defined in the
-- Pantry.Error module becuase that would introduce a circular
-- dependency.
module Pantry.Food ( 
  -- * Data types within Foods
  -- ** FoodId
  FoodId ( FoodId, unFoodId ),
  zeroFoodId,
  oneFoodId,
  
  -- ** Name
  Name ( Name, unName ),
  
  -- ** Grams
  Grams ( Grams, unGrams),
  MixedGrams ( MixedGrams, unMixedGrams ),
  
  -- ** Nutrients
  NutAmt ( NutAmt, unNutAmt ),
  NutNameAmt ( NutNameAmt, nutName, nutAmt ),
  NutNamesAmts ( NutNamesAmts, unNutNamesAmts ),
  NutsPerG ( NutsPerG, unNutsPerG ),
  NutNamesPerGs ( NutNamesPerGs, unNutNamesPerGs ),
  setQtyByNut,

  -- ** Units
  UnitNameAmt ( UnitNameAmt, unitName, unitGrams ),
  UnitNamesAmts ( UnitNamesAmts, unUnitNamesAmts ),
  absGrams,
  absOunces,
  absPounds,
  addUnit,
  deleteUnit,
  allAvailUnits,
  allUnits,
  changeCurrUnit,
  
  -- ** Tags
  TagVal ( TagVal, unTagVal ),
  TagNameVal ( TagNameVal, tagName, tagVal ),
  TagNamesVals ( TagNamesVals, unTagNamesVals ),
  hasTag,
  getTag,
  tagPred,
  changeTag,
  deleteTag,
  foodMatch,
  
  -- ** Quantity, yield, ingredients, PctRefuse
  Qty ( Qty, unQty ),
  changeQty,
  Yield ( AutoYield, ExplicitYield ),
  PctRefuse ( PctRefuse, unPctRefuse ),
  setPctRefuse,
  minusPctRefuse,
  Ingr ( Ingr, unIngr ),
  ingredientMass,
  recipeYield,
  addIngredient,
  deleteIngredients,
  
  -- ** NutRatio
  NutRatio ( NutRatio, unNutRatio ),
  nutRatio,
  
  -- * Food
  Food ( Food, tags, units, nutsPerGs, currUnit,
         pctRefuse, qty, yield, ingr, foodId ),
  emptyFood,
  
  -- * Queries on foods
  foodGrams,
  nutsPerGToAmt,
  foodIngrNuts,
  foodNuts,
  getNut,
  foodAbsNuts,
  sumNuts,
  foldNuts,
  foldFoodNuts,
  
  -- * Generic functions
  deleteMapKeys,
  
  -- * Typeclasses
  -- ** HasName
  HasName ( name )
  ) where
  
import qualified Data.Map as M
import Data.Ratio
import Data.Maybe
import Control.Monad
import Data.List
import qualified Data.Foldable as F
import Pantry.Types
import Data.Text(Text, pack)
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Pantry.Exact(Exact(exact))
import Pantry.Rounded(Rounded)
import Data.Serialize (Serialize(put, get), putWord8)
import Data.Monoid as Monoid
import Data.Word ( Word8 )

type Matcher = Text -> Bool

-- * Data types within Foods

-- | A food's unique identifier. Do not make FoodId an instance of
-- Enum. This would allow prec to be called on it. In theory this
-- would be OK (prec can be partial) but better to avoid that. Instead
-- use the Next typeclass.
newtype FoodId = FoodId { unFoodId :: NonNegInteger }
                 deriving (Show, Eq, Ord, Next, Serialize)

-- | FoodID of zero
zeroFoodId :: FoodId
zeroFoodId = FoodId . partialNewNonNegInteger $ (0 :: Int)

-- | FoodID of one
oneFoodId :: FoodId
oneFoodId = FoodId . partialNewNonNegInteger $ (1 :: Int)

class HasName a where
  name :: a -> Name

-- | The name of a tag, nutrient, or unit
newtype Name = Name { unName :: Text } deriving (Eq, Ord, Show, Exact)
instance Serialize Name where
  put (Name t) = put . encodeUtf8 $ t
  get = get >>= return . Name . decodeUtf8

-- | Grams expressed as a simple NonNeg number.
newtype Grams = Grams { unGrams :: NonNeg }
                deriving (Eq, Ord, Show, Add,
                          HasZero, Exact, Rounded, Serialize)

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

-- | setQtyByNut can fail in a multitude of ways so this data type
-- indicates the various failures.
data SetQtyByNutFailure

  = QBNNoMatchingNut
    -- ^ No nutrients matched the matcher given
    
  | QBNMultipleMatchingNuts [Name]
    -- ^ Multiple nutrients matched the pattern given

  | QBNNutIsZero Name
    -- ^ One nutrient matched but the value of that nutrient is
    -- zero. (Not returned if the requested food amount is zero; that
    -- computation will succeed and set the food's quantity to zero.)

-- | Given a matcher and a quantity, set the food's quantity so that
-- the amount of the given nutrient is what was given. See
-- documentation for SetQtyByNutResult for details.
setQtyByNut :: (Text -> Bool)
               -> NutAmt
               -> Food
               -> Either SetQtyByNutFailure Food
setQtyByNut = undefined

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
data Yield = AutoYield
              | ExplicitYield MixedGrams
              deriving Show

instance Serialize Yield where
  put AutoYield = putWord8 0
  put (ExplicitYield m) = putWord8 1 >> put m
  get = do
    c <- get
    case (c :: Word8) of
      0 -> return AutoYield
      1 -> do
        m <- get
        return $ ExplicitYield (MixedGrams m)
      _ -> fail "non-matching number"

-- | Ingredients
newtype Ingr = Ingr { unIngr :: [Food] }
             deriving (Show, Serialize, Monoid)

-- * Other datatypes

-- | NutRatio is not within the Food datatype, but reports use it. For
-- now this seems to be the best module to put this in.
newtype NutRatio = NutRatio { unNutRatio :: NonNeg }
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
                 , yield = AutoYield
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

-- | Changes the value of a tag. Supply this function with the exact
-- name of the tag (not a pattern) and the new value of the tag. If
-- the tag already had a value, it is replaced with the new value.
changeTag :: TagNameVal -> Food -> Food
changeTag (TagNameVal n (TagVal v)) f = f {tags = new} where
  (TagNamesVals old) = tags f
  new = TagNamesVals (M.insert n (TagVal v) old)

deleteTag :: (Text -> Bool) -> Food -> Food
deleteTag m f = f { tags = new } where
  (TagNamesVals old) = tags f
  new = TagNamesVals . M.fromList . filter p . M.assocs $ old
  p (Name n, _) = not $ m n

changeQty :: Qty -> Food -> Food
changeQty q f = f { qty = q }

-- | True if food has a tag whose name matches the Name and whose
-- value matches the second matcher.
foodMatch :: Name -> (Text -> Bool) -> Food -> Bool
foodMatch n v f = isJust $ do
  (TagNameVal _ (TagVal val)) <- getTag n f
  guard $ v val

-- |Add an arbitrary unit to a food.
addUnit :: UnitNameAmt -> Food -> Food
addUnit (UnitNameAmt n a) f = f {units = new} where
  (UnitNamesAmts old) = units f
  new = UnitNamesAmts $ M.insert n a old

-- | Delete arbitrary units whose name matches a matcher.
deleteUnit :: (Text -> Bool) -> Food -> Food
deleteUnit m f = f {units = new} where
  (UnitNamesAmts old) = units f
  new = UnitNamesAmts $ M.fromList ns
  ns = filter (not . m . uN . fst) (M.assocs old)
  uN n = let (Name v) = n in v

allAvailUnits :: Food -> UnitNamesAmts
allAvailUnits = allUnits . units

allUnits :: UnitNamesAmts -> UnitNamesAmts
allUnits (UnitNamesAmts m) = UnitNamesAmts $ M.fromList new where
  new = ars ++ absU
  ars = M.assocs m
  absU = map (\(UnitNameAmt n a) -> (n, a))
         [absGrams, absOunces, absPounds]

-- | Change current unit to the one matching a matcher. Fails if there
-- is not exactly one available unit that matches; Left will hold a
-- list with the number of matches (might be zero, might be two or
-- more).
changeCurrUnit :: (Text -> Bool) -> Food -> Either [Name] Food
changeCurrUnit m f = let
  p ((Name n), _) = m n
  matches = filter p .
            M.assocs .
            unUnitNamesAmts .
            allUnits .
            units $ f
  in case matches of
    [] -> Left []
    (x:[]) -> let
      nameAmt = uncurry UnitNameAmt x
      newFood = f { currUnit = nameAmt }
      in Right newFood
    xs -> Left (map fst xs)

foodGrams :: Food -> Grams
foodGrams f = Grams $ q `mult` u where
  (Qty quan) = qty f
  q = case quan of
    (Left nn) -> nn
    (Right mix) -> toNonNeg mix
  (UnitNameAmt _ (Grams u)) = currUnit f

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
recipeYield f = case yield f of
  (ExplicitYield e) -> Just . Grams . toNonNeg . unMixedGrams $ e
  AutoYield -> let m = unGrams (ingredientMass f) in
    case m > zero of
      False -> Nothing
      True -> Just . Grams $ m
{-
recipeYield f = if' (isJust y) gr i where
  gr = Just . Grams . toNonNeg . (\(MixedGrams m) -> m) . fromJust $ y
  (Yield y) = yield f
  i = if' (mR > zero) (Just . Grams $ mR) Nothing
  (Grams mR) = ingredientMass f
-}

-- Ingredient functions

-- | Appends an ingredient to a Food's ingredients.
addIngredient :: Food    -- ^ The ingredient to add
                 -> Food -- ^ The food to add it to
                 -> Food
addIngredient i f = f {ingr = Ingr new} where
  (Ingr old) = ingr f
  new = old ++ [i]
  
deleteIngredients :: Food -> Food
deleteIngredients f = f {ingr = Ingr [] }

if' :: Bool -> a -> a -> a
if' b x y = case b of True -> x; False -> y
