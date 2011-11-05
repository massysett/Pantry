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
              | Arbitrary String
              deriving Show
data PctRefuse = PctRefuse Decimal deriving Show
data Qty = Qty MixedNum deriving Show
data Yield = Yield (Maybe Decimal) deriving Show
data Ingr = Ingr [Food] deriving Show

data Nut = Nut String Rational
data Unit = Unit String Rational
data Tag = Tag String String

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

addTag :: Food -> Tag -> Food
addTag f (Tag n v) = f { tags = newTags} where
  (Tags oldMap) = tags f
  newTags = Tags (M.insert n v oldMap)

{-
matchingTags :: Food -> String -> [Tag]
matchingTags f s = catMaybes . map (getTag f) tags $ matches where
  matches = filter p 
-}

getTag :: Food -> String -> Maybe Tag
getTag f s = do
  let (Tags m) = tags f
  v <- M.lookup s m
  return $ Tag s v

