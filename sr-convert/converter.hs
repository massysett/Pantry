-- | Converts USDA SR database to a script that can be used as a
-- Pantry input script.
--
-- Unicode handling - Text files in SR are encoded in
-- ISO-8859-1. LibZip will unzip the files into ByteStrings. Because
-- ISO-8859-1 is a single-byte encoding, there is no reason to
-- transform it from the ByteString into a Unicode format (such as
-- [Char], or Data.Text) so it is simply left as is for processing
-- with Parsec. Therefore, upon output, ByteString would be in
-- ISO-8859-1. That's not so good. Thus this script uses the Haskell
-- binding to iconv to convert the ByteString from ISO-8859-1 to
-- UTF-8. That means that the output script will always be in UTF-8,
-- even if the system's default encoding is different.
--
-- Unlike previous incarnations of this script, this version makes no
-- effort to rename nutrients, remove useless units, etc. This task is
-- left for further processing within Pantry.

module Main where

import qualified Data.Char as C
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map as M
import qualified Codec.Text.IConv as I
import qualified Codec.Archive.Zip as Z
import Data.Map ((!))
import qualified Data.Set as S
import Text.Parsec
import Data.List (foldl')
import Data.Maybe ( catMaybes )
import System.Environment ( getArgs )

type NutId = BS.ByteString
type NutName = BS.ByteString
type NutUnits = BS.ByteString
type NutAmt = BS.ByteString
type Ndb = BS.ByteString
type UnitDesc = BS.ByteString
type UnitWeight = BS.ByteString
type UnitCount = BS.ByteString
type FoodName = BS.ByteString
type GroupId = BS.ByteString
type GroupName = BS.ByteString

type FoodDesBS = BS.ByteString
type GroupBS = BS.ByteString
type NutsBS = BS.ByteString
type UnitsBS = BS.ByteString
type NutDefBS = BS.ByteString

type Parser = Parsec BS.ByteString ()

main :: IO ()
main = do
  as <-getArgs
  bs <- getByteStrings (head as)
  let unconv = getUnconvertedOutput bs
  o <- iso8859toUtf8 unconv
  BS.putStr o

textDelim :: Char
textDelim = '~'

fieldDelim :: Char
fieldDelim = '^'

-- | Takes a file path to an SR zip file and returns all the
-- bytestrings of the various files inside the zip. Isolated within a
-- single function to make it easy to change zip libraries (or
-- possibly to even simply use unzip at the command line and then read
-- in the bytestrings.)
getByteStrings :: FilePath -- ^ Path to ZIP file
                  -> IO (FoodDesBS, GroupBS, NutsBS, UnitsBS, NutDefBS)
getByteStrings f = do
  bs <- BS.readFile f
  let a = Z.toArchive bs
      e = flip entry a
  return ( e "FOOD_DES.txt",
           e "FD_GROUP.txt",
           e "NUT_DATA.txt",
           e "WEIGHT.txt",
           e "NUTR_DEF.txt" )

-- | Gets an entry from a Codec.Archive.Zip archive. Aborts program if
-- entry not found.
entry :: FilePath -> Z.Archive -> BS.ByteString
entry p a = case Z.findEntryByPath p a of
  Nothing -> error $ "entry " ++ p ++ " not found"
  (Just e) -> Z.eCompressedData e

-- | Takes all output and converts it to UTF-8. Isolated here to make
-- it easy to switch conversion libraries. Returns value in the IO
-- monad because, although the Haskell iconv bindings are pure, maybe
-- not all converters are.
iso8859toUtf8 :: BS.ByteString -> IO BS.ByteString
iso8859toUtf8 bs = return new where
  new = I.convert iso utf bs
  iso = "ISO-8859-1"
  utf = "UTF8"

-- | Takes all ByteStrings and returns output that is encoded in
-- ISO-8859-1.
getUnconvertedOutput ::
  (FoodDesBS, GroupBS, NutsBS, UnitsBS, NutDefBS)
  -> BS.ByteString
getUnconvertedOutput (f, g, n, u, nd) = let
  pf = parseFoodFile f
  pg = parseGroupFile g
  pn = parseNutFile n
  pu = parseUnitsFile u
  pnd = parseNutrDefFile nd
  fs = makeFoods pf pg pn pu pnd
  in BS8.append header . BS8.concat . map printFood $ fs
  
-- | A hopefully helpful header for the rest of the output.
header :: BS.ByteString
header = BS8.unlines . map BS8.pack $ [
  "#!/bin/sh"
  , "# This file created by converter.hs."
  , "# It converts a USDA SR zip file to a script that can be used"
  , "# to input foods to Pantry."
  , "# This file is encoded in UTF-8, regardless of the default"
  , "# system encoding."
  , ""
  ]

text :: Parser BS.ByteString
text = do
  b <- between (char textDelim) (char textDelim) (many notControl)
  return . BS8.pack $ b

notControl :: Parser Char
notControl = noneOf $ fieldDelim : textDelim : "\r\n"

type NutFileMap = M.Map Ndb (M.Map NutId NutAmt)
type GroupMap = M.Map GroupId GroupName
type Food = (Ndb, GroupName, FoodName, [(NutName, NutUnits, NutAmt)],
             [(UnitCount, UnitDesc, UnitWeight)])

printFood :: Food -> BS.ByteString
printFood (ndb, groupName, foodName, nutList, unitList) = s where
  s = (BS8.unlines . map addSlash $ ls) `BS8.snoc` '\n'
  addSlash bs = BS.append bs (BS8.pack " \\")
  space = BS8.singleton ' '
  ls = first ++ nuts ++ units ++ end
  first =
    [ BS8.pack "pantry --clear --create"
    , BS8.pack "--change-tag ndb " `BS.append` ndb
    , BS8.pack "--change-tag group " `BS.append` groupName
    , BS8.pack "--change-tag name " `BS.append` foodName
    , BS8.pack "--change-quantity 100"
    , BS8.pack "--set-unit grams 1" ]
  nuts = map toNut nutList
  toNut (n, u, a) = BS8.intercalate space [opt, na, a] where
    opt = BS8.pack "--add-nutrient"
    na = n `BS8.append` (BS8.pack ", ") `BS8.append` a
  units = catMaybes . map toUnit $ unitList where
    toUnit (c, d, w)
      | c /= BS8.singleton '1' = Nothing
      | otherwise = let opt = BS8.pack "--add-available-unit"
                    in Just . BS8.intercalate space $ [opt, d, w]
  end = [ BS8.pack "--append" ]

makeFoods ::
  [(Ndb, GroupId, FoodName)]
  -> GroupMap
  -> NutFileMap
  -> M.Map Ndb [(UnitCount, UnitDesc, UnitWeight)]
  -> M.Map NutId (NutName, NutUnits)
  -> [Food]
makeFoods fs gs ns us ds = map toFood fs where
  toFood (ndb, groupId, foodName) = (ndb, groupName, foodName,
                                     nutList, unitList) where
    groupName = gs ! groupId
    nutList = map f . M.assocs . (! ndb) $ ns where
      f (nutId, nutAmt) = (nutName, nutUnits, nutAmt) where
        (nutName, nutUnits) = ds ! nutId
    unitList = us ! ndb

parseFoodFile :: BS.ByteString -> [(Ndb, GroupId, FoodName)]
parseFoodFile s = case parse (many foodRecord) "FOOD_DES.txt" s of
  (Left err) -> error $ "could not parse foodDes: " ++ show err
  (Right ls) -> ls

parseGroupFile :: BS.ByteString -> GroupMap
parseGroupFile s = case parse groupFile "FD_GROUP.txt" s of
  (Left err) -> error $ "could not parse group file: " ++ show err
  (Right ls) -> M.fromList ls

parseNutFile :: BS.ByteString -> M.Map Ndb (M.Map NutId NutAmt)
parseNutFile s = case parse nutFile "NUT_DATA.txt" s of
  (Left err) -> error $ "could not parse nutData: " ++ show err
  (Right ls) -> foldl' nutFileMapFold M.empty ls

parseUnitsFile :: BS.ByteString
                  -> M.Map Ndb [(UnitCount, UnitDesc, UnitWeight)]
parseUnitsFile bs = case parse unitFile "WEIGHT.txt" bs of
  (Left err) -> error $ "could not parse WEIGHT.txt: " ++ show err
  (Right ls) -> let
    folder m (n, c, d, w) = M.insertWith (++) n [(c, d, w)] m
    in foldl' folder M.empty ls

parseNutrDefFile :: BS.ByteString -> M.Map NutId (NutName, NutUnits)
parseNutrDefFile bs = case parse nutDefFile "nutrDef" bs of
  (Left err) -> error $ "could not parse nutrDef: " ++ show err
  (Right ls) -> nutDefMap ls

foodRecord :: Parser (Ndb, GroupId, FoodName)
foodRecord = do
  n <- text
  _ <- char fieldDelim
  g <- text
  _ <- char fieldDelim
  f <- text
  _ <- manyTill anyChar (string "\r\n")
  return (n, g, f)

groupFile :: Parser [(GroupId, GroupName)]
groupFile = many groupRecord

groupRecord :: Parser (GroupId, GroupName)
groupRecord = do
  i <- text                    -- 1
  _ <- char fieldDelim
  n <- text                    -- 2
  _ <- string "\r\n"
  return (i, n)

-- | Add a new NutId and NutAmt to an inner NutFileMap.
alterInner :: NutId
              -> NutAmt
              -> M.Map NutId NutAmt
              -> M.Map NutId NutAmt
alterInner  i a m = M.insert i a m

-- | Add a new Ndb, NutId, and NutAmt to the outer NutFileMap.
alterOuter :: Ndb
              -> NutId
              -> NutAmt
              -> M.Map Ndb (M.Map NutId NutAmt)
              -> M.Map Ndb (M.Map NutId NutAmt)
alterOuter n i a m = M.alter f n m where
  f may = case may of
    Nothing -> Just (M.singleton i a)
    (Just im) -> Just $ alterInner i a im

nutFileMapFold :: M.Map Ndb (M.Map NutId NutAmt)
                  -> (Ndb, NutId, NutAmt)
                  -> M.Map Ndb (M.Map NutId NutAmt)
nutFileMapFold m (n, i, a) = alterOuter n i a m

nutFile :: Parser [(Ndb, NutId, NutAmt)]
nutFile = many nutRecord

nutRecord :: Parser (Ndb, NutId, NutAmt)
nutRecord = do
  n <- text               -- 1
  _ <- char fieldDelim
  i <- text               -- 2
  _ <- char fieldDelim
  a <- many notControl    -- 3
  _ <- manyTill anyChar (string "\r\n")
  return (n, i, BS8.pack a)

unitFile :: Parser [(Ndb, UnitCount, UnitDesc, UnitWeight)]
unitFile = many unitRecord

unitRecord :: Parser (Ndb, UnitCount, UnitDesc, UnitWeight)
unitRecord = do
  n <- text                    -- 1
  _ <- char fieldDelim
  _ <- many notControl         -- 2
  _ <- char fieldDelim
  c <- many notControl         -- 3
  _ <- char fieldDelim
  d <- text                    -- 4
  _ <- char fieldDelim
  w <- many notControl         -- 5
  _ <- char fieldDelim
  _ <- many notControl         -- 6
  _ <- char fieldDelim
  _ <- many notControl         -- 7
  _ <- string "\r\n"
  return (n, BS8.pack c, d, BS8.pack w)

nutDefRecord :: Parser (NutId, NutName, NutUnits)
nutDefRecord = do
  fs <- sepBy (text) (char fieldDelim)
  _ <- string "\r\n"
  let (i:u:_:n:_) = fs
  return (i, n, u)

nutDefFile :: Parser [(NutId, NutName, NutUnits)]
nutDefFile = many nutDefRecord

nutDefMap :: [(NutId, NutName, NutUnits)]
             -> M.Map NutId (NutName, NutUnits)
nutDefMap = M.fromList . map toPair where
  toPair (i, n, u) = (i, (n, u))

