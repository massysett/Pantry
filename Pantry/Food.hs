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
  getFoodId,
  setFoodId,
  
  -- ** Grams
  Grams ( Grams, unGrams),
  MixedGrams ( MixedGrams, unMixedGrams ),
  PosMixedGrams ( PosMixedGrams, unPosMixedGrams),

  -- ** Nutrients
  NutName ( NutName, unNutName ),
  NutAmt ( NutAmt, unNutAmt ),
  getNuts,
  setNuts,
  SetQtyByNutFailure( QBNNoMatchingNut, QBNMultipleMatchingNuts,
                      QBNNutIsZero ),
  setQtyByNut,
  
  -- ** Units
  UnitName ( UnitName, unUnitName ),
  UnitAmt ( UnitAmt, unUnitAmt ),
  CurrUnit ( CurrUnit, currUnitName, currUnitAmt ),
  getUnits,
  setUnits,
  getCurrUnit,
  setCurrUnit,
  
  -- ** Tags
  TagName ( TagName, unTagName ),
  TagVal ( TagVal, unTagVal ),
  getTags,
  setTags,
  
  -- ** Quantity
  Qty ( Qty, unQty ),
  getQty,
  setQty,
  
  -- ** Yield
  Yield ( AutoYield, ExplicitYield ),
  setYield,
  getYieldGrams,
  
  -- ** Ingredients
  Ingr ( Ingr, unIngr ),
  getIngr,
  setIngr,
  
  -- ** PctRefuse
  PctRefuse ( PctRefuse, unPctRefuse ),
  getPctRefuse,
  setPctRefuse,
  minusPctRefuse,
  
  -- * The Food datatype
  Food,
  emptyFood,

  -- * Calculations
  sumNuts,
  
  -- * Typeclasses
  HasText( toText ),
  
  -- * Utilities
  matches,
  deleteMapKeys,
  changeCurrUnit,
  foodGrams
  ) where
  
import qualified Data.Map as M
import Data.Ratio
import Data.Maybe
import Control.Monad
import Data.List
import qualified Data.Foldable as F
import Pantry.Types as T
import Data.Text(Text, pack)
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Pantry.Exact(Exact(exact))
import Pantry.Rounded(Rounded)
import Data.Serialize (Serialize(put, get), putWord8)
import Data.Monoid as Monoid
import Data.Word ( Word8 )
import Control.Applicative((<*>), (*>), pure, liftA2)

type Matcher = Text -> Bool

------------------------------------------------------------
-- FOODID
------------------------------------------------------------
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

-- | Gets the FoodID of a food.
getFoodId :: Food -> FoodId
getFoodId = foodId

-- | Sets the FoodID of a food
setFoodId :: FoodId -> Food -> Food
setFoodId i f = f { foodId = i }

-- | Grams expressed as a simple NonNeg number.
newtype Grams = Grams { unGrams :: NonNeg }
                deriving (Eq, Ord, Show, Add,
                          HasZero, Exact, Rounded, Serialize,
                          HasNonNeg)

-- | Grams expressed as a mixed number.
newtype MixedGrams = MixedGrams { unMixedGrams :: NonNegMixed }
                     deriving (Show, Exact, Serialize, HasNonNeg)

-- | Grams that must be positive (that is, greater than zero.)
newtype PosMixedGrams =
  PosMixedGrams { unPosMixedGrams :: PosMixed }
  deriving (Show, Serialize, HasPos, HasNonNeg)

------------------------------------------------------------
-- NUTRIENTS
------------------------------------------------------------

-- | The name of a nutrient
newtype NutName = NutName { unNutName :: Text }
                  deriving (Eq, Ord, Show)

instance Serialize NutName where
  put (NutName n) = put . encodeUtf8 $ n
  get = get >>= return . NutName . decodeUtf8

-- | The amount of a nutrient
newtype NutAmt = NutAmt { unNutAmt :: T.NonNeg }
                 deriving (Eq, Ord, Show, Serialize)

-- | The amount of a nutrient per gram of food.
newtype NutPerG = NutPerG { unNutPerG :: T.NonNeg }
                  deriving (Eq, Ord, Show, Serialize)

-- | Get the current nutrients in a food. These will be properly
-- scaled depending on the current amount of the food, the
-- ingredients, and the yield. Nutrients that are explicitly set with
-- setNuts will take precedence over those that are in the
-- ingredients.
getNuts :: Food -> M.Map NutName NutAmt
getNuts = undefined

-- | Set the nutrients in a food. These will be properly scaled when
-- stored inside the food. This computation fails if the current mass
-- of the food is zero because then the scaling mechanism would fail.
setNuts :: M.Map NutName NutAmt -> Food -> Maybe Food
setNuts = undefined

-- | setQtyByNut can fail in a multitude of ways so this data type
-- indicates the various failures.
data SetQtyByNutFailure

  = QBNNoMatchingNut
    -- ^ No nutrients matched the matcher given
    
  | QBNMultipleMatchingNuts [NutName]
    -- ^ Multiple nutrients matched the pattern given

  | QBNNutIsZero NutName
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


------------------------------------------------------------
-- UNITS
------------------------------------------------------------

-- | The name of a unit
newtype UnitName = UnitName { unUnitName :: Text }
                   deriving (Show, Eq, Ord)
instance Serialize UnitName where
  put (UnitName n) = put . encodeUtf8 $ n
  get = get >>= return . UnitName . decodeUtf8

-- | The amount of a unit
newtype UnitAmt = UnitAmt { unUnitAmt :: PosMixedGrams }
                  deriving (Show, Serialize, HasPos)

-- | A food's current unit.
data CurrUnit = CurrUnit { currUnitName :: UnitName,
                           currUnitAmt :: UnitAmt }
                deriving Show

instance Serialize CurrUnit where
  get = liftA2 CurrUnit get get
  put (CurrUnit n a) = put n *> put a

-- | Get the available units in a food.
getUnits :: Food -> M.Map UnitName UnitAmt
getUnits = units

-- | Set the available units in a food.
setUnits :: M.Map UnitName UnitAmt -> Food -> Food
setUnits m f = f { units = m }

-- | Gets the current unit that is set in a food. Might or might not
-- be in the map that is returned by getUnits.
getCurrUnit :: Food -> CurrUnit
getCurrUnit = currUnit

-- | Sets the current unit of a food. Might be in the map that is
-- returned in getUnits, but it does not have to be.
setCurrUnit :: CurrUnit -> Food -> Food
setCurrUnit c f = f { currUnit = c }

------------------------------------------------------------
-- TAGS
------------------------------------------------------------

-- | The name of a tag
newtype TagName = TagName { unTagName :: Text }
                  deriving (Show, Eq, Ord)
instance Serialize TagName where
  get = get >>= return . TagName . decodeUtf8
  put (TagName t) = put . encodeUtf8 $ t

-- | The value of a tag
newtype TagVal = TagVal { unTagVal :: Text }
               deriving (Eq, Ord, Show, Exact)
instance Serialize TagVal where
  get = get >>= return . TagVal . decodeUtf8
  put (TagVal v) = put . encodeUtf8 $ v

-- | All of a food's tags
getTags :: Food -> M.Map TagName TagVal
getTags = tags

-- | Set the tags in a food
setTags :: M.Map TagName TagVal -> Food -> Food
setTags m f = f { tags = m }

------------------------------------------------------------
-- QUANTITY
------------------------------------------------------------
-- | A food's quantity. If this was human input, it will be a
-- NonNegMixed. If it was computed, it will be a NonNeg.
newtype Qty = Qty { unQty :: (Either NonNeg NonNegMixed) }
            deriving (Show, Serialize)

instance Exact Qty where
  exact (Qty q) = either exact exact q

getQty :: Food -> Qty
getQty = qty

setQty :: Qty -> Food -> Food
setQty q f = f { qty = q }

------------------------------------------------------------
-- YIELD
------------------------------------------------------------
-- | A food's recipe yield. If this was input by the user, it will be Just
-- MixedGrams. If not input, or if explicitly unset, it will be
-- Nothing; then Pantry will compute the yield.
data Yield = AutoYield
              | ExplicitYield PosMixedGrams
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
        return $ ExplicitYield (PosMixedGrams m)
      _ -> fail "non-matching number"

-- | Sets the yield of a food.
setYield :: Yield -> Food -> Food
setYield y f = f { yield = y }

-- | Get the yield of the recipe, in grams, if the food has a
-- yield. If the food has an explicit yield, return that. If the food
-- an AutoYield, and the ingredients have mass, return that. If the
-- food is an AutoYield, and its ingredients have no mass, return
-- Nothing.
getYieldGrams :: Food -> Maybe Grams
getYieldGrams f = case yield f of
  (ExplicitYield pmg) -> Just . Grams . T.toNonNeg $ pmg
  AutoYield -> let
    gr = ingrGrams (ingr f)
    in case gr > T.zero of
      True -> Just gr
      False -> Nothing

------------------------------------------------------------
-- INGREDIENTS
------------------------------------------------------------

-- | Ingredients
newtype Ingr = Ingr { unIngr :: [Food] }
             deriving (Show, Serialize, Monoid)

getIngr :: Food -> Ingr
getIngr = ingr

setIngr :: Ingr -> Food -> Food
setIngr i f = f { ingr = i }

-- | Returns the mass in grams of the ingredients.
ingrGrams :: Ingr -> Grams
ingrGrams (Ingr fds) = foldl' f (Grams T.zero) fds where
  f g fd = g `T.add` (foodGrams fd)

------------------------------------------------------------
-- PCT REFUSE
------------------------------------------------------------
-- | The percentage of refuse in a food.
newtype PctRefuse = PctRefuse {unPctRefuse :: BoundedPercent }
                    deriving (Eq, Ord, Show, HasZero, Exact, Serialize)

getPctRefuse :: Food -> PctRefuse
getPctRefuse = pctRefuse

setPctRefuse :: PctRefuse -> Food -> Food
setPctRefuse p f = f { pctRefuse = p }

minusPctRefuse :: Food -> Food
minusPctRefuse f = f {qty = newQty} where
  newQty = Qty (Left . subtractPercent q $ p)
  (Qty quan) = qty f
  q = case quan of
    (Left nn) -> nn
    (Right mx) -> toNonNeg mx
  (PctRefuse p) = pctRefuse f

------------------------------------------------------------
-- FOOD
------------------------------------------------------------
data Food = Food { 
  foodId :: FoodId
  , nutsPerG :: M.Map NutName NutPerG
  , units :: M.Map UnitName UnitAmt
  , currUnit :: CurrUnit                            
  , tags :: M.Map TagName TagVal
  , qty :: Qty
  , yield :: Yield  
  , ingr :: Ingr  
  , pctRefuse :: PctRefuse } deriving Show

instance Serialize Food where
  put f = put (foodId f)
          *> put (nutsPerG f)
          *> put (units f)
          *> put (currUnit f)
          *> put (tags f)
          *> put (qty f)
          *> put (yield f)
          *> put (ingr f)
          *> put (pctRefuse f)
  get = pure Food
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get

emptyFood :: CurrUnit -> Food
emptyFood c = Food { 
  foodId = zeroFoodId
  , nutsPerG = M.empty
  , units = M.empty
  , currUnit = c
  , tags = M.empty
  , qty = Qty (Left T.zero)
  , yield = AutoYield
  , ingr = Ingr []
  , pctRefuse = T.zero
  }
    
-- | Add the nutrients in two maps of nutrients.
sumNuts :: M.Map NutName NutAmt
           -> M.Map NutName NutAmt
           -> M.Map NutName NutAmt
sumNuts = undefined

-- | Typeclass for Text wrappers.
class HasText a where
  toText :: a -> Text

instance HasText NutName where toText = unNutName
instance HasText UnitName where toText = unUnitName
instance HasText TagName where toText = unTagName
instance HasText TagVal where  toText = unTagVal

------------------------------------------------------------
-- UTILITIES
------------------------------------------------------------
matches :: HasText a => (Text -> Bool) -> a -> Bool
matches f a = f (toText a)

-- | Delete all items from a map whose names match a matcher.
deleteMapKeys :: (Ord a, HasText a)
                 => (Text -> Bool)
                 -> M.Map a v
                 -> M.Map a v
deleteMapKeys p = M.fromList . filter p' . M.assocs where
  p' (a, _) = not $ p (toText a)

-- | Change current unit to the one matching a matcher. Fails if there
-- is not exactly one available unit that matches; Left will hold a
-- list with the number of matches (might be zero, might be two or
-- more).
changeCurrUnit :: (Text -> Bool) -> Food -> Either [UnitName] Food
changeCurrUnit m f = let
  p ((UnitName n), _) = m n
  matches = filter p .
            M.assocs .
            units $ f
  in case matches of
    [] -> Left []
    (x:[]) -> let
      nameAmt = uncurry CurrUnit x
      newFood = f { currUnit = nameAmt }
      in Right newFood
    xs -> Left (map fst xs)

-- | Find the current weight in grams of this food.
foodGrams :: Food -> Grams
foodGrams f = Grams $ q `T.mult` u where
  (Qty quan) = qty f
  q = case quan of
    (Left nn) -> nn
    (Right mix) -> T.toNonNeg mix
  (CurrUnit _ (UnitAmt pmg)) = currUnit f
  u = T.toNonNeg pmg
