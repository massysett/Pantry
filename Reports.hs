module Reports where

import Food
import Data.List
import Data.List.Split

data Report = Report { header :: [Food] -> String
                     , body :: [Food] -> Food -> String
                     , footer :: [Food] -> String }

name :: Report
name = Report { header = (\_ -> "")
              , footer = (\_ -> "")
              , body = b } where
  b _ f = n ++ "\n" where
    n = case getTag t f of
      Nothing -> "(No name)"
      (Just (TagNameVal _ (TagVal v))) -> show v
    t = Name "name"

tags :: Report
tags = Report { header = (\_ -> "")
              , footer = (\_ -> "")
              , body = b } where
  b = undefined

columns :: Int -> [a] -> [[a]]
columns n ls = transpose . splitEvery s $ ls where
  s = if r == 0 then q else q + 1
  (q, r) = length ls `divMod` n

rpad :: Int -> String -> String
rpad l s = s ++ p where
  p | length s >= l = " "
    | otherwise = replicate (l - length s) ' '

colsToString :: [Int] -> [String] -> String
colsToString is ss = firsts ++ lasts ++ "\n" where
  firsts = concatMap (uncurry rpad) . zip is $ ss
  lasts = concat . drop (length is) $ ss

colsListToString :: [Int] -> [[String]] -> String
colsListToString = concatMap . colsToString
