module Food where

import qualified Data.Map as M
import Data.Ratio
import Data.Decimal
import Text.Regex.PCRE
import Data.Maybe

data Nuts = Nuts (M.Map String Rational) deriving Show
data Units = Units(M.Map String Rational) deriving Show
data Tags = Tags (M.Map String String) deriving Show
data MixedNum = MixedNum Decimal Rational deriving Show
data AbsUnit = Grams | Ounces | Pounds deriving Show
data CurrUnit = Absolute AbsUnit
              | Arbitrary Unit
              deriving Show
data PctRefuse = PctRefuse Decimal deriving Show
data Qty = Qty MixedNum deriving Show
data Yield = Yield (Maybe Decimal) deriving Show
data Ingr = Ingr [Food] deriving Show

data Nut = Nut String Rational deriving Show
data Unit = Unit String Rational deriving Show
data Tag = Tag String String deriving Show

data Food = Food { tags :: Tags
                 , units :: Units
                 , nuts :: Nuts
                 , currUnit :: CurrUnit
                 , pctRefuse :: PctRefuse
                 , qty :: Qty
                 , yield :: Yield
                 , ingr :: Ingr } deriving Show

emptyFood :: Food
emptyFood = Food { tags = Tags M.empty
                 , units = Units M.empty
                 , nuts = Nuts M.empty
                 , currUnit = Absolute Grams
                 , pctRefuse = PctRefuse (Decimal 0 0)
                 , qty = Qty (MixedNum (Decimal 0 100) (0 % 1))
                 , yield = Yield Nothing
                 , ingr = Ingr [] }

-- Tag manipulations

addTag :: Food -> Tag -> Food
addTag f (Tag n v) = f { tags = newTags} where
  (Tags oldMap) = tags f
  newTags = if null v
            then Tags (M.delete n oldMap)
            else Tags (M.insert n v oldMap)

tagMatches :: String -- ^ Tag name
              -> String -- ^ Regexp for value
              -> Food
              -> Bool
tagMatches n p f = case (getTag f n) of Nothing -> False
                                        (Just (Tag _ v)) -> v =~ p

hasTag :: String -> Food -> Bool
hasTag s f = isJust (getTag f s)

getTagList :: Food -> [Tag]
getTagList f = map (uncurry Tag) (M.assocs ts) where
  (Tags ts) = tags f

getTag :: Food -> String -> Maybe Tag
getTag f s = do
  let (Tags m) = tags f
  v <- M.lookup s m
  return $ Tag s v

-- Units manipulations
addUnit :: Food -> Unit -> Food
addUnit f (Unit n v) = f {units = newUnits} where
  (Units oldMap) = units f
  newUnits = Units $ if v /= 0
                     then M.insert n v oldMap
                     else M.delete n oldMap

getUnitList :: Food -> [Unit]
getUnitList f = map (uncurry Unit) (M.assocs m) where
  (Units m) = units f

allUnitsMap :: Units -> M.Map String (Either AbsUnit Unit)
allUnitsMap (Units m) = M.fromList eithers where
  eithers = arbs ++ abss
  arbs = zip arbKeys arbVals
  arbKeys = M.keys m
  arbVals = map Right . map (uncurry Unit) $ M.assocs m
  absKeys = ["g", "oz", "lb"]
  absVals = map Left [Grams, Ounces, Pounds]
  abss = zip absKeys absVals

if' :: Bool -> a -> a -> a
if' b x y = case b of True -> x; False -> y

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

data Error = NoMatchingUnit
           | MultipleMatchingUnits [String]
