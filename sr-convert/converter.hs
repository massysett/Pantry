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

module Main (main) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map as M
import qualified Codec.Text.IConv as I
import qualified Codec.Archive.Zip as Z
import Data.Map ((!))
import Text.Parsec
import System.Environment ( getArgs )
import Codec.Compression.Zlib.Raw ( decompress )
import Text.ShellEscape
import Control.Monad.Identity
import qualified Data.ByteString as BSS

type NutId = BS.ByteString
type NutName = BS.ByteString
type NutUnits = BS.ByteString
type Ndb = BS.ByteString
type GroupId = BS.ByteString
type GroupName = BS.ByteString

type Parser = Parsec BS.ByteString ()
type ParserSt = ParsecT BS.ByteString (M.Map Ndb [BS.ByteString]) Identity

mapToBs :: (Ord k) => M.Map k BS.ByteString -> BS.ByteString
mapToBs = BS.concat . M.elems

concatValues :: (Ord k) => M.Map k [BS.ByteString] -> M.Map k BS.ByteString
concatValues = M.map f where
  f = foldr BS.append (BS8.pack "\n")

appendSlashes :: (Ord k) => M.Map k [BS.ByteString] -> M.Map k [BS.ByteString]
appendSlashes m = M.map f m where
  f = map g
  g bs = bs `BS8.append` (BS8.pack " \\\n")

textDelim :: Char
textDelim = '~'

fieldDelim :: Char
fieldDelim = '^'


quote :: BS.ByteString -> BS.ByteString
quote = BS.fromChunks
        . return
        . bytes
        . (escape :: BSS.ByteString -> Sh)
        . BSS.concat
        . BS.toChunks

pkunzip :: Z.Archive -> String -> BS.ByteString
pkunzip a s = decompress . entry s $ a

main :: IO ()
main = do
  as <- getArgs
  bs <- BS.readFile (head as)
  BS.putStr (makeText bs)

makeText :: BS.ByteString -- ^ Data from ZIP file
            -> BS.ByteString -- ^ UTF-8 output
makeText bs =
  toUtf8
  . addHeader
  . unMap
  . addFoods a
  . addWeights a
  . addNuts
  $ a
  where
    a = Z.toArchive bs

addNuts :: 
  Z.Archive
  -> M.Map Ndb [BS.ByteString]
addNuts a = let
  bsDef = pkunzip a "NUTR_DEF.txt"
  bsData = pkunzip a "NUT_DATA.txt"
  defs = parseNutrDefFile bsDef
  p = do
    _ <- many (nutRecordSt defs)
    eof
    getState
  in case runParser p M.empty "" bsData of 
    (Left err) -> error $ "could not add nuts: " ++ show err
    (Right st) -> st

addWeights :: 
  Z.Archive
  -> M.Map Ndb [BS.ByteString]
  -> M.Map Ndb [BS.ByteString]
addWeights a m = let
  p = do
    _ <- many unitRecordSt
    eof
    getState
  bs = pkunzip a "WEIGHT.txt"
  in case runParser p m "" bs of
    (Left err) -> error $ "could not add weights: " ++ show err
    (Right st) -> st

addFoods ::
  Z.Archive
  -> M.Map Ndb [BS.ByteString]
  -> M.Map Ndb [BS.ByteString]
addFoods a m = let
  bsFood = pkunzip a "FOOD_DES.txt"
  groupMap = parseGroupFile (pkunzip a "FD_GROUP.txt")
  p = do
    _ <- many (foodRecordSt groupMap)
    eof
    getState
  in case runParser p m "" bsFood of
    (Left err) -> error $ "could not add foods: " ++ show err
    (Right st) -> st

unMap :: M.Map Ndb [BS.ByteString]
         -> BS.ByteString
unMap = mapToBs . concatValues . appendSlashes

addHeader :: BS.ByteString -> BS.ByteString
addHeader bs = header `BS.append` bs

toUtf8 :: BS.ByteString -> BS.ByteString
toUtf8 = I.convert iso utf where
  iso = "ISO-8859-1"
  utf = "UTF8"

-- | Gets an entry from a Codec.Archive.Zip archive. Aborts program if
-- entry not found.
entry :: FilePath -> Z.Archive -> BS.ByteString
entry p a = case Z.findEntryByPath p a of
  Nothing -> error $ "entry " ++ p ++ " not found"
  (Just e) -> Z.eCompressedData e

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

text :: (Monad m) => ParsecT BS.ByteString u m BS.ByteString
text = do
  b <- between (char textDelim) (char textDelim) (many notControl)
  return . BS8.pack $ b

notControl :: (Monad m) => ParsecT BS.ByteString u m Char
notControl = noneOf $ fieldDelim : textDelim : "\r\n"

type GroupMap = M.Map GroupId GroupName

spc :: BS.ByteString
spc = BS8.singleton ' '

parseGroupFile :: BS.ByteString -> GroupMap
parseGroupFile s = case parse groupFile "FD_GROUP.txt" s of
  (Left err) -> error $ "could not parse group file: " ++ show err
  (Right ls) -> M.fromList ls

parseNutrDefFile :: BS.ByteString -> M.Map NutId (NutName, NutUnits)
parseNutrDefFile bs = case parse nutDefFile "nutrDef" bs of
  (Left err) -> error $ "could not parse nutrDef: " ++ show err
  (Right ls) -> nutDefMap ls

foodRecordSt :: GroupMap -> ParserSt ()
foodRecordSt gm = do
  n <- text
  _ <- char fieldDelim
  g <- text
  _ <- char fieldDelim
  f <- text
  skipMany (noneOf "\r\n")
  _ <- string "\r\n"
  let first = [ BS8.pack "pantry --clear --create"
              , BS8.pack "--change-tag ndb " `BS.append` n
              , BS8.pack "--change-tag group " `BS.append` (quote groupName)
              , BS8.pack "--change-tag name " `BS.append` (quote f)
              , BS8.pack "--change-quantity 100"
              , BS8.pack "--set-unit grams 1" ]
      groupName = gm ! g
  modifyState (prependLines n first)

prependLines :: Ndb
                -> [BS.ByteString]
                -> M.Map Ndb [BS.ByteString]
                -> M.Map Ndb [BS.ByteString]
prependLines n as = M.alter f n where
  f maybeV = let
    lst = case maybeV of
      Nothing -> [BS8.pack "--append"]
      (Just ls) -> ls
    in Just $ foldr (:) lst as

groupFile :: Parser [(GroupId, GroupName)]
groupFile = many groupRecord

groupRecord :: Parser (GroupId, GroupName)
groupRecord = do
  i <- text                    -- 1
  _ <- char fieldDelim
  n <- text                    -- 2
  _ <- string "\r\n"
  return (i, n)

nutRecordSt :: M.Map NutId (NutName, NutUnits)
               -> ParserSt ()
nutRecordSt dm = do
  ndb <- text               -- 1
  _ <- char fieldDelim
  i <- text               -- 2
  _ <- char fieldDelim
  a <- many notControl    -- 3
  skipMany (noneOf "\r\n")
  _ <- string "\r\n"
  let nutStr = BS8.intercalate spc [opt, (quote na), (BS8.pack a)]
      opt = BS8.pack "--add-nutrient"
      na = n `BS8.append` (BS8.pack ", ") `BS8.append` u
      (n, u) = dm ! i
  modifyState (prependLines ndb [nutStr])
  return ()

unitRecordSt :: ParserSt ()
unitRecordSt = do
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
  let opt = BS8.pack "--add-available-unit"
      txt = BS8.intercalate spc [opt, (quote d), BS8.pack w]
  case c == "1" of
    False -> return ()
    True -> modifyState (prependLines n [txt]) >> return ()

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

