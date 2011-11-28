module Reports.Columns ( rpad
                       , txtColWidth
                       , numColWidth
                       , fmtColumnRow
                       , newspaper ) where

import Prelude(Int, map, zip, ($), (.), divMod, uncurry,
               length, (==), (+), drop)
import Data.Text (Text, append, snoc, justifyLeft, concat)
import Data.List (transpose)
import Data.List.Split (splitEvery)

txtColWidth :: Int
txtColWidth = 35

numColWidth :: Int
numColWidth = 6

-- | Pad a string on the right so that the result string is at least a
-- given number of columns wide.
rpad :: Int -> Text -> Text
rpad l = justifyLeft l ' '

-- | Formats a single line for columnar text. For colsToString is ts,
-- is is the number of columns minus 1, and ts is the list of items to
-- place into columns. Each column width (except the last column) is
-- specified in is.
fmtColumnRow :: [Int] -> [Text] -> Text
fmtColumnRow is ss = firsts `append` lasts `snoc` '\n' where
  firsts = concat . map (uncurry rpad) . zip is $ ss
  lasts = concat . drop (length is) $ ss


-- | For colsToString is ts, ts is a nested list of strings. Each
-- inner list is a line of items that has already been placed into
-- columns. is specifies the width of each column (except the last
-- column).
colsListToString :: [Int] -> [[Text]] -> Text
colsListToString is tss = concat . map (fmtColumnRow is) $ tss

-- | Arrange a single list of items into newspaper-style columns.
arrangeNews :: Int      -- ^ How many columns
           -> [a]   -- ^ List of items
           -> [[a]] -- ^ Each inner list is a single line to display,
                    -- containing mulitple columns.
arrangeNews n ls = transpose . splitEvery s $ ls where
  s = if r == 0 then q else q + 1
  (q, r) = length ls `divMod` n

-- | Takes a single list of items and formats it into newspaper
-- columns. For listToCols is ts, the number of columns will be
-- length is - 1. The width of each column, except for the last
-- column, is specified in is. (The last column simply gets a newline
-- appended to the end.)
newspaper :: [Int] -> [Text] -> Text
newspaper ls = colsListToString ls . arrangeNews (length ls + 1)

