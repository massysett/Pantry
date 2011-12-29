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
  renameNuts,
  deleteNuts,
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
  foodGrams,
  foodMatch
  ) where
  
import qualified Data.Map as M
import Data.List ( foldl' )
import qualified Pantry.Types as T
import Data.Text(Text)
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Pantry.Exact(Exact(exact))
import Pantry.Rounded(Rounded)
import Data.Serialize (Serialize(put, get), putWord8)
import Data.Monoid ( Monoid )
import Data.Word ( Word8 )
import Control.Applicative((<*>), (*>), pure, liftA2)
import Data.Maybe ( fromMaybe, catMaybes )
import Control.DeepSeq ( NFData(rnf), deepseq )

------------------------------------------------------------
-- FOODID
------------------------------------------------------------
-- | A food's unique identifier. Do not make FoodId an instance of
-- Enum. This would allow prec to be called on it. In theory this
-- would be OK (prec can be partial) but better to avoid that. Instead
-- use the Next typeclass.
newtype FoodId = FoodId { unFoodId :: T.NonNegInteger }
                 deriving (Show, Eq, Ord, T.Next, Serialize,
                           Exact, NFData)

-- | FoodID of zero
zeroFoodId :: FoodId
zeroFoodId = FoodId . T.partialNewNonNegInteger $ (0 :: Int)

-- | FoodID of one
oneFoodId :: FoodId
oneFoodId = FoodId . T.partialNewNonNegInteger $ (1 :: Int)

-- | Gets the FoodID of a food.
getFoodId :: Food -> FoodId
getFoodId = foodId

-- | Sets the FoodID of a food
setFoodId :: FoodId -> Food -> Food
setFoodId i f = f { foodId = i }

-- | Grams expressed as a simple NonNeg number.
newtype Grams = Grams { unGrams :: T.NonNeg }
                deriving (Eq, Ord, Show, T.Add,
                          T.HasZero, Exact, Rounded, Serialize,
                          T.HasNonNeg, T.Divide, NFData)

-- | Grams expressed as a mixed number.
newtype MixedGrams = MixedGrams { unMixedGrams :: T.NonNegMixed }
                     deriving (Show, Exact, Serialize, T.HasNonNeg,
                               NFData)

-- | Grams that must be positive (that is, greater than zero.)
newtype PosMixedGrams =
  PosMixedGrams { unPosMixedGrams :: T.PosMixed }
  deriving (Show, Serialize, T.HasPos, T.HasNonNeg, Exact, Eq,
            T.FromStr, NFData)

------------------------------------------------------------
-- NUTRIENTS
------------------------------------------------------------

-- | The name of a nutrient
newtype NutName = NutName { unNutName :: Text }
                  deriving (Eq, Ord, Show, Exact, NFData)

instance Serialize NutName where
  put (NutName n) = put . encodeUtf8 $ n
  get = get >>= return . NutName . decodeUtf8

-- | The amount of a nutrient.
newtype NutAmt = NutAmt { unNutAmt :: T.NonNeg }
                 deriving (Eq, Ord, Show, Serialize,
                           T.Add, T.Divide, Rounded,
                           T.FromStr, NFData)

-- | The amount of a nutrient per gram of food.
newtype NutPerG = NutPerG T.NonNeg
                  deriving (Eq, Ord, Show, Serialize, NFData)

-- | Portions explicit nutrients for the portion size of the food.
portionNutPerG :: Grams -- ^ Weight of food
                  -> NutPerG
                  -> PortionedNutAmt
portionNutPerG (Grams g) (NutPerG perG) =
  PortionedNutAmt $ g `T.mult` perG

-- | Converts a PortionedNutAmt to a NutAmt. The NutAmt is the tidy
-- representation that the API uses (otherwise the NutAmt is not used
-- in the Food.hs module).
toNutAmt :: PortionedNutAmt -> NutAmt
toNutAmt = NutAmt . unPortionedNutAmt

toPortionedNutAmt :: NutAmt -> PortionedNutAmt
toPortionedNutAmt = PortionedNutAmt . unNutAmt

-- | Creates a computation that will take a PortionedNutAmt and
-- converts it to a NutPerG. Will fail to produce a computation if
-- Grams is zero. (This function is written this way so that the
-- resulting computation can be used with Data.Map.Map.)
toNutPerG :: Grams -- ^ Weight of food
             -> Maybe (PortionedNutAmt -> NutPerG)
toNutPerG (Grams g) = do
  p <- T.nonNegToPos g
  let f (PortionedNutAmt na) = NutPerG $ na `T.nonNegDivPos` p
  return f

-- | Get the current nutrients in a food. These will be properly
-- scaled and portioned depending on the current amount of the food, the
-- ingredients, and the yield. Nutrients that are explicitly set with
-- setNuts will take precedence over those that are in the
-- ingredients.
getNuts :: Food -> M.Map NutName NutAmt
getNuts f = M.union explNuts ingrNuts where
  foodGr = foodGrams f
  explNuts = M.map toNutAmt
             . M.map (portionNutPerG foodGr)
             . nutsPerG
             $ f
  ingrNuts = M.map toNutAmt
             $ fromMaybe M.empty (foodPortionedNuts f)

-- | Set the nutrients in a food. These will be properly portioned
-- when stored inside the food. This computation fails if the current
-- mass of the food is zero because then the portioning mechanism
-- would fail.
setNuts :: M.Map NutName NutAmt -> Food -> Maybe Food
setNuts m f = do
  c <- toNutPerG $ foodGrams f
  let newNuts = M.map c . M.map toPortionedNutAmt $ m
  return $ f { nutsPerG = newNuts }

-- | Rename nutrients. This computation is provided (in addition to
-- setNuts) because unlike setNuts this computation always succeeds.
renameNuts :: (Text -> Bool) -- ^ Nutrient name to match
              -> NutName     -- ^ New name
              -> Food
              -> Food
renameNuts m n f = f { nutsPerG = new } where
  old = nutsPerG f
  changer a@((NutName na), v) = case m na of
    True -> (n, v)
    False -> a
  new = M.fromList
        . map changer
        . M.assocs
        $ old
        
-- | Delete nutrients. This computation is provided (in addition to
-- setNuts) because unlike setNuts this computation always succeeds.
deleteNuts :: (Text -> Bool) -- ^ Delete nutrients matching this pattern
              -> Food
              -> Food
deleteNuts m f = f { nutsPerG = new } where
  old = nutsPerG f
  changer a@((NutName na), _) = case m na of
    True -> Just a
    False -> Nothing
  new = M.fromList
        . catMaybes
        . map changer
        . M.assocs
        $ old

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
setQtyByNut m (NutAmt a) f = let
  p ((NutName n), _) = m n
  ms = filter p . M.assocs . nutsPerG $ f
  in case ms of
    [] -> Left QBNNoMatchingNut
    (((NutName n), (NutPerG perG)):[]) -> case T.divide a perG of
      Nothing -> Left . QBNNutIsZero . NutName $ n
      (Just g) -> let
        newQ = Qty
               . Left
               . T.nonNegDivPos g
               . currUnitAmt
               . currUnit
               $ f
        in Right f { qty = newQ }
    xs -> Left . QBNMultipleMatchingNuts . map fst $ xs

------------------------------------------------------------
-- UNITS
------------------------------------------------------------

-- | The name of a unit
newtype UnitName = UnitName { unUnitName :: Text }
                   deriving (Show, Eq, Ord, Exact, NFData)
instance Serialize UnitName where
  put (UnitName n) = put . encodeUtf8 $ n
  get = get >>= return . UnitName . decodeUtf8

-- | The amount of a unit
newtype UnitAmt = UnitAmt { unUnitAmt :: PosMixedGrams }
                  deriving (Show, Serialize, T.HasPos, Exact, Eq,
                            T.FromStr, NFData)

-- | A food's current unit.
data CurrUnit = CurrUnit { currUnitName :: UnitName,
                           currUnitAmt :: UnitAmt }
                deriving Show

instance Serialize CurrUnit where
  get = liftA2 CurrUnit get get
  put (CurrUnit n a) = put n *> put a

instance NFData CurrUnit where
  rnf (CurrUnit n a) = n `deepseq` a `deepseq` ()

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
                  deriving (Show, Eq, Ord, Exact, NFData)
instance Serialize TagName where
  get = get >>= return . TagName . decodeUtf8
  put (TagName t) = put . encodeUtf8 $ t

-- | The value of a tag
newtype TagVal = TagVal { unTagVal :: Text }
               deriving (Eq, Ord, Show, Exact, NFData)
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
newtype Qty = Qty { unQty :: (Either T.NonNeg T.NonNegMixed) }
            deriving (Show, Serialize, NFData)

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

instance NFData Yield where
  rnf AutoYield = ()
  rnf (ExplicitYield pmg) = pmg `deepseq` ()

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
             deriving (Show, Serialize, Monoid, NFData)

getIngr :: Food -> Ingr
getIngr = ingr

setIngr :: Ingr -> Food -> Food
setIngr i f = f { ingr = i }

-- | Returns the mass in grams of the ingredients.
ingrGrams :: Ingr -> Grams
ingrGrams (Ingr fds) = foldl' f (Grams T.zero) fds where
  f g fd = g `T.add` (foodGrams fd)

-- | Unportioned nutrients of the ingredients. These must be portioned
-- depending on the yield of the recipe.
newtype UnportionedNutAmt = UnportionedNutAmt T.NonNeg
                          deriving (Eq, Ord, Show, T.Add, NFData)

-- | After a nutrient is scaled, it must be portioned--that is,
-- adjusted depending on the weight of the food. Scaling is adjusting
-- for the yield; portioning is adjusting for the portion size.
newtype PortionedNutAmt = PortionedNutAmt { unPortionedNutAmt :: T.NonNeg }
                       deriving (Eq, Ord, Show, T.Add, NFData)

-- | Gets all the unportioned nutrients of a food. Simply converts the
-- NutAmts to UnportionedNutAmts.
foodUnportionedNuts :: Food -> M.Map NutName UnportionedNutAmt
foodUnportionedNuts = M.map (UnportionedNutAmt . unNutAmt ) . getNuts

-- | Gets all the unportioned nutrients of a list of ingredients.
ingrUnportionedNuts :: Ingr -> M.Map NutName UnportionedNutAmt
ingrUnportionedNuts (Ingr fs) = foldl' c M.empty fs where
  c m f = M.unionWith T.add m (foodUnportionedNuts f)

-- | The portion factor is a ratio of the food's weight in grams to
-- the actual yield of the food.
newtype PortionFactor = PortionFactor T.NonNeg
                        deriving (Eq, Ord, Show, T.HasNonNeg, NFData)

-- | Gets the portion factor. If the food's actual yield is
-- zero--either because the ingredients all have zero quantity or
-- because there are no ingredients--this computation fails.
portionFactor :: Grams -- ^ Weight of the food
                 -> Ingr
                 -> Yield
                 -> Maybe PortionFactor
portionFactor g i y = do
  let actualYield = case y of
        AutoYield -> ingrGrams i
        (ExplicitYield pmg) -> Grams . T.toNonNeg $ pmg
  r <- g `T.divide` actualYield
  return . PortionFactor . unGrams $ r

-- | Portion a scaled nutrient.
portionUnportionedNutrient :: PortionFactor
                         -> UnportionedNutAmt
                         -> PortionedNutAmt
portionUnportionedNutrient (PortionFactor f) (UnportionedNutAmt s) =
  PortionedNutAmt $ s `T.mult` f
   
-- | Gets the portioned nutrients of a food. If this computation
-- fails, the food has no portioned nutrients--it might have no
-- ingredients, or the mass of all the ingredients might be zero.
foodPortionedNuts :: Food -> Maybe (M.Map NutName PortionedNutAmt)
foodPortionedNuts f = do
  portionFac <- portionFactor (foodGrams f) (ingr f) (yield f)
  return
    . M.map (portionUnportionedNutrient portionFac)
    . ingrUnportionedNuts
    . ingr
    $ f

------------------------------------------------------------
-- PCT REFUSE
------------------------------------------------------------
-- | The percentage of refuse in a food.
newtype PctRefuse = PctRefuse {unPctRefuse :: T.BoundedPercent }
                    deriving (Eq, Ord, Show, T.HasZero, Exact, Serialize,
                              NFData)

getPctRefuse :: Food -> PctRefuse
getPctRefuse = pctRefuse

setPctRefuse :: PctRefuse -> Food -> Food
setPctRefuse p f = f { pctRefuse = p }

minusPctRefuse :: Food -> Food
minusPctRefuse f = f {qty = newQty} where
  newQty = Qty (Left . T.subtractPercent q $ p)
  (Qty quan) = qty f
  q = case quan of
    (Left nn) -> nn
    (Right mx) -> T.toNonNeg mx
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

instance NFData Food where
  rnf f = foodId f `deepseq`
          nutsPerG f `deepseq`
          units f `deepseq`
          currUnit f `deepseq`
          tags f `deepseq`
          qty f `deepseq`
          yield f `deepseq`
          ingr f `deepseq`
          pctRefuse f `deepseq`
          ()

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
sumNuts = M.unionWith T.add

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
  ms = filter p .
       M.assocs .
       units $ f
  in case ms of
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

-- | Returns True if the food has a tag with TagName whose value
-- matches the function given.
foodMatch :: TagName -> (Text -> Bool) -> Food -> Bool
foodMatch n m f = case M.lookup n (tags f) of
  Nothing -> False
  (Just (TagVal v)) -> m v
