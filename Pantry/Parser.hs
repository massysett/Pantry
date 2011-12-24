module Pantry.Parser (getConveyor) where

import qualified Pantry.Tray as T
import qualified Control.Monad.Error as E
import Pantry.Error(Error)
import qualified Pantry.Error as R
import Pantry.Radio.Messages ( Request, args )
import System.Console.OptParse.OptParse (
  OptDesc(OptDesc), ArgDesc(Flag, Single, Double, Variable),
  parseOptsArgs)
import qualified Data.Map as M
import qualified Pantry.Matchers as Matchers
import Data.Text ( Text, pack )
import qualified Pantry.Conveyor as C
import qualified Pantry.Reports.Types as RT
import qualified Pantry.Food as F
import Control.Monad ((>=>))
import Pantry.Types ( fromStr, NonNegInteger, NonNegMixed )
import Pantry.Reports ( buildReportGroups, printReportGroups )
import qualified Pantry.Sorter as S
import Data.List ( isPrefixOf )
import qualified Pantry.Paths as P

getConveyor :: Request
               -> T.Tray
               -> E.ErrorT Error IO T.Tray
getConveyor r t = do
  let as = args r
  case parse as of
    (Left e) -> E.throwError e
    (Right opts) -> (conveyor opts) t

parse :: [String] -> Either Error Opts
parse ss = do
  let noPosArgs ps = case ps of
        [] -> Right []
        ps -> Left (R.NonOptionArguments (map snd ps))
  r <- parseOptsArgs optDescs defaultOpts noPosArgs ss
  return . fst $ r

optDescs :: [OptDesc Opts Error]
optDescs = [
  ignoreCase
  , caseSensitive
  , invertOpt
  , noInvert
  , within
  , posix
  , pcre
  , exact
  , find
  , findIds
  , clear
  , recopy
  , Pantry.Parser.head
  , Pantry.Parser.tail
  , create
  , move
  , undo
  , changeTag
  , deleteTag
  , setCurrUnit
  , changeQty
  , changePctRefuse
  , changeYield
  , removeYield
  , byNutrient
  , refuse
  , addNut
  , changeNut
  , renameNut
  , deleteNut
  , addAvailUnit
  , changeAvailUnit
  , renameAvailUnit
  , deleteAvailUnit
  , replaceWithIngr
  , removeIngr
  , ingrToVolatile
  , Pantry.Parser.print
  , goal
  , showAllNuts
  , showTag
  , showAllTags
  , oneColumn
  , key
  , order
  , append
  , prepend
  , replace
  , edit
  , delete
  , ingrFromVolatile
  , open
  , Pantry.Parser.appendFile
  , prependFile
  , close
  , save
  , saveAs
  , quit
  , help
  , version
  , copyright
  ]

data Opts = Opts {
  sensitive :: Matchers.CaseSensitive,
  invert :: Bool,
  matcher :: String -> Either Error (Text -> Bool),
  conveyor :: T.Tray -> E.ErrorT Error IO T.Tray,
  reportOpts :: RT.ReportOpts,
  tagMap :: S.TagMap }

defaultOpts :: Opts
defaultOpts = let
  win = Matchers.within (Matchers.CaseSensitive False)
  defaultWithin s = return (win s)
  in Opts { sensitive = Matchers.CaseSensitive False
          , invert = False
          , matcher = defaultWithin
          , conveyor = return
          , reportOpts = RT.defaultReportOpts
          , tagMap = M.empty }

ignoreCase :: OptDesc Opts Error
ignoreCase = OptDesc "i" ["ignore-case"] a where
  a = Flag (\o -> return $ o { sensitive = Matchers.CaseSensitive False })

caseSensitive :: OptDesc Opts Error
caseSensitive = OptDesc "" ["case-sensitive"] a where
  a = Flag (\o -> return $ o { sensitive = Matchers.CaseSensitive True })

invertOpt :: OptDesc Opts Error
invertOpt = OptDesc "v" ["invert"] a where
  a = Flag (\o -> return $ o { invert = True })

noInvert :: OptDesc Opts Error
noInvert = OptDesc "" ["no-invert"] a where
  a = Flag (\o -> return $ o { invert = False })

flipCase :: 
  Bool  -- ^ Invert matching behavior?
  -> (String -> Either Error (Text -> Bool))
  -> String -> Either Error (Text -> Bool)
flipCase b f s = case b of
  True -> do
    m <- f s
    return (\t -> not (m t))
  False -> f s

within :: OptDesc Opts Error
within = OptDesc "" ["within"] a where
  a = Flag f
  f o = return $ o { matcher = newMatcher } where
    newMatcher = flipCase (invert o)
                 (raiseMatcher (Matchers.within (sensitive o)))

posix :: OptDesc Opts Error
posix = OptDesc "" ["posix"] a where
  a = Flag f
  f o = return $ o { matcher = new } where
    new = flipCase (invert o) (Matchers.tdfa (sensitive o))

pcre :: OptDesc Opts Error
pcre = OptDesc "" ["pcre"] a where
  a = Flag f
  f o = return $ o { matcher = new } where
    new = flipCase (invert o) (Matchers.pcre (sensitive o))

exact :: OptDesc Opts Error
exact = OptDesc "" ["exact"] a where
  a = Flag f
  f o = return $ o { matcher = new } where
    new = flipCase (invert o)
          (raiseMatcher (Matchers.exact (sensitive o)))

raiseMatcher ::
  (String -> Text -> Bool)
  -> String -> Either Error (Text -> Bool)
raiseMatcher f s = Right $ f s

addConveyor :: Opts -> (T.Tray -> E.ErrorT Error IO T.Tray) -> Opts
addConveyor o c = o { conveyor = conveyor o >=> c }

-- Filtering
find :: OptDesc Opts Error
find = OptDesc "f" ["find"] a where
  a = Double f
  f o a1 a2 = do
    m <- (matcher o) a2    
    let p = F.foodMatch (F.TagName . pack $ a1) m
        c = C.trayFilterToConvey .
            C.filterToTrayFilter .
            C.predToFilter $ p
        newO = addConveyor o c
    return newO

strToId :: String -> Either R.Error F.FoodId
strToId s = case fromStr s of
  Nothing -> Left $ R.IDStringNotValid s
  (Just i) -> Right $ F.FoodId i

findIds :: OptDesc Opts Error
findIds = OptDesc "" ["id"] a where
  a = Variable f
  f o as = do
    is <- mapM strToId as
    let newO = addConveyor o c
        c = C.trayMToConvey . C.filterMToTrayM $ volatileFilter
        volatileFilter = C.findIds is
    return newO

clear :: OptDesc Opts Error
clear = OptDesc "" ["clear"] a where
  a = Flag f
  f o = return $ addConveyor o c where
    c = C.newVolatileToConvey C.clear

recopy :: OptDesc Opts Error
recopy = OptDesc "" ["recopy"] a where
  a = Flag f
  f o = return $ addConveyor o c where
    c = C.trayFilterToConvey C.recopy

strToNonNegInteger :: String -> Either R.Error NonNegInteger
strToNonNegInteger s = case fromStr s of
  Nothing -> Left (R.NonNegIntegerStringNotValid s)
  (Just i) -> Right i

head :: OptDesc Opts Error
head = OptDesc "" ["head"] a where
  a = Single f
  f o a1 = do
    i <- strToNonNegInteger a1
    return . addConveyor o . C.filterToConvey $ C.head i

tail :: OptDesc Opts Error
tail = OptDesc "" ["tail"] a where
  a = Single f
  f o a1 = do
    i <- strToNonNegInteger a1
    return . addConveyor o . C.filterToConvey $ C.tail i

create :: OptDesc Opts Error
create = OptDesc "" ["create"] a where
  a = Flag f
  f o = return . addConveyor o . C.filterToConvey $ C.create

move :: OptDesc Opts Error
move = OptDesc "" ["move"] a where
  a = Variable f
  f o as = case as of
    [] -> Left R.NoMoveIDsGiven
    (_:[]) -> Left R.OneMoveIDGiven
    (as1:ass) -> do
      first <- case as1 of
        "f" -> return C.Beginning
        "F" -> return C.Beginning
        n -> do
          i <- strToNonNegInteger n
          return (C.After (F.FoodId i))
      rest <- do
        is <- mapM strToNonNegInteger ass
        return $ map F.FoodId is
      let volatileChanger = C.move first rest
          c = C.trayMToConvey . C.filterMToTrayM $ volatileChanger
      return $ addConveyor o c

undo :: OptDesc Opts Error
undo = OptDesc "" ["undo"] a where
  a = Single f
  f o a1 = do
    i <- strToNonNegInteger a1
    let u = C.undo i
        c = C.trayMToConvey u
    return $ addConveyor o c

------------------------------------------------------------
-- CHANGE TAGS AND PROPERTIES
------------------------------------------------------------
changeTag :: OptDesc Opts Error
changeTag = OptDesc "c" ["change-tag"] a where
  a = Double f
  f o a1 a2 = return newO where
    n = F.TagName . pack $ a1
    v = F.TagVal . pack $ a2
    changeTag fd = F.setTags newTags fd where
      oldTags = F.getTags fd
      newTags = M.insert n v oldTags
    ct = changeTag
    c = C.xformToConvey (return . ct)
    newO = addConveyor o c

deleteTag :: OptDesc Opts Error
deleteTag = OptDesc "" ["delete-tag"] a where
  a = Single f
  f o a1 = matcher o a1 >>= \m ->
    let xformer = deleteTag
        deleteTag fd = F.setTags new fd where
          old = F.getTags fd
          p ((F.TagName n), _) = not . m $ n
          new = M.fromList
                . filter p
                . M.assocs
                $ old
        c = C.xformToConvey (return . xformer)
    in return $ addConveyor o c

setCurrUnit :: OptDesc Opts Error
setCurrUnit = OptDesc "u" ["match-unit"] a where
  a = Single f
  f o a1 = matcher o a1 >>= \m ->
    let changeWithErr fd = case F.changeCurrUnit m fd of
          (Right good) -> Right good
          (Left err) -> Left (R.NotExactlyOneMatchingUnit err)
        c = C.xformToConvey changeWithErr
    in return $ addConveyor o c

changeQty :: OptDesc Opts Error
changeQty = OptDesc "q" ["change-quantity"] a where
  a = Single f
  f o a1 = case fromStr a1 of
    Nothing -> Left (R.NonNegMixedNotValid a1)
    (Just n) ->
      let q = F.Qty (Right (n :: NonNegMixed))
          changeQty fd = F.setQty q fd
          xform = changeQty
          c = C.xformToConvey (return . xform)
      in return $ addConveyor o c

changePctRefuse :: OptDesc Opts Error
changePctRefuse = OptDesc "" ["change-percent-refuse"] a where
  a = Single f
  f o a1 = case fromStr a1 of
    Nothing -> Left (R.BoundedPercentNotValid a1)
    (Just n) ->
      let xform = F.setPctRefuse (F.PctRefuse n)
          c = C.xformToConvey (return . xform)
      in return $ addConveyor o c

changeYield :: OptDesc Opts Error
changeYield = OptDesc "" ["change-yield"] a where
  a = Single f
  f o a1 = case fromStr a1 of
    Nothing -> Left (R.NonNegMixedNotValid a1)
    (Just n) -> return $ addConveyor o c where
      c = C.xformToConvey (return . setYield)
      setYield fd = F.setYield y fd where
        y = F.ExplicitYield . F.PosMixedGrams $ n

removeYield :: OptDesc Opts Error
removeYield = OptDesc "" ["remove-yield"] a where
  a = Flag f
  f o = return $ addConveyor o c where
    c = C.xformToConvey (return . setYield)
    setYield fd = F.setYield F.AutoYield fd

byNutrient :: OptDesc Opts Error
byNutrient = OptDesc "" ["by-nutrient"] a where
  a = Double f
  f o a1 a2 = do
    m <- matcher o a1
    q <- case fromStr a2 of
      Nothing -> Left (R.NonNegMixedNotValid a2)
      (Just g) -> Right g
    let setQ f = case F.setQtyByNut m q f of
          (Left err) -> Left (R.QByNutFail err)
          (Right good) -> Right good
        c = C.xformToConvey setQ
    return $ addConveyor o c

refuse :: OptDesc Opts Error
refuse = OptDesc "r" ["refuse"] a where
  a = Flag f
  f o = return
        . addConveyor o
        . C.xformToConvey
        $ (return . F.minusPctRefuse)

addNut :: OptDesc Opts Error
addNut = OptDesc "" ["add-nutrient"] a where
  a = Double f
  f o a1 a2 = case fromStr a2 of
    Nothing -> Left (R.NonNegMixedNotValid a2)
    (Just nn) -> let
      n = F.NutName . pack $ a1
      v = F.NutAmt nn
      adder fd = let
        old = F.getNuts fd
        new = M.insert n v old
        in case F.setNuts new fd of
          Nothing -> Left $ R.AddNutError fd
          (Just fd') -> Right fd'
      in Right
         . addConveyor o
         . C.xformToConvey
         $ adder

changeNut :: OptDesc Opts Error
changeNut = OptDesc "" ["change-nutrient"] a where
  a = Double f
  f o a1 a2 = do
    m <- matcher o a1
    q <- case fromStr a2 of
      Nothing -> Left (R.NonNegMixedNotValid a2)
      (Just nn) -> Right (F.NutAmt nn)
    let changer fd = let
          old = F.getNuts fd
          nutChanger e@(nn@(F.NutName n), _) = case m n of
            True -> (nn, q)
            False -> e
          new = M.fromList
                . map nutChanger
                . M.assocs
                $ old
          in case F.setNuts new fd of
            (Nothing) -> Left $ R.AddNutError fd
            (Just fd') -> Right fd'
    return
      . addConveyor o
      . C.xformToConvey
      $ changer

renameNut :: OptDesc Opts Error
renameNut = OptDesc "" ["rename-nutrient"] a where
  a = Double f
  f o a1 a2 = do
    m <- matcher o a1
    let n = F.NutName . pack $ a2
    return
      . addConveyor o
      . C.xformToConvey
      $ (return . F.renameNuts m n)

deleteNut :: OptDesc Opts Error
deleteNut = OptDesc "" ["delete-nutrients"] a where
  a = Single f
  f o a1 = do
    m <- matcher o a1
    return
      . addConveyor o
      . C.xformToConvey
      $ (return . F.deleteNuts m )

addAvailUnit :: OptDesc Opts Error
addAvailUnit = OptDesc "" ["add-available-unit"] a where
  a = Double f
  f o a1 a2 = do
    let n = F.UnitName . pack $ a1
    v <- case fromStr a2 of
      Nothing -> Left (R.PosMixedNotValid a2)
      (Just pm) -> Right . F.UnitAmt $ pm
    let adder fd = F.setUnits new fd where
          old = F.getUnits fd
          new = M.insert n v old
    return
      . addConveyor o
      . C.xformToConvey
      $ (return . adder)

changeAvailUnit :: OptDesc Opts Error
changeAvailUnit = OptDesc "" ["change-avail-unit"] a where
  a = Double f
  f o a1 a2 = do
    m <- matcher o a1
    v <- case fromStr a2 of
      Nothing -> Left (R.PosMixedNotValid a2)
      (Just pm) -> Right . F.UnitAmt $ pm
    let mapfn (F.UnitName n) amt = case m n of
          True -> v
          False -> amt
        changer fd = F.setUnits new fd where
          old = F.getUnits fd
          new = M.mapWithKey mapfn old
    return
      . addConveyor o
      . C.xformToConvey
      $ (return . changer)

-- TODO on all renames, fail if more than one thing is renamed
renameAvailUnit :: OptDesc Opts Error
renameAvailUnit = OptDesc "" ["rename-avail-unit"] a where
  a = Double f
  f o a1 a2 = do
    m <- matcher o a1
    let v = F.UnitName . pack $ a2
        mapfn u@(F.UnitName n) = case m n of
          True -> v
          False -> u
        changer fd = F.setUnits new fd where
          old = F.getUnits fd
          new = M.mapKeys mapfn old
    return
      . addConveyor o
      . C.xformToConvey
      $ (return . changer)

deleteAvailUnit :: OptDesc Opts Error
deleteAvailUnit = OptDesc "" ["delete-avail-unit"] a where
  a = Single f
  f o a1 = do
    m <- matcher o a1
    let fltr (F.UnitName n) _ = not . m $ n
        changer fd = F.setUnits new fd where
          old = F.getUnits fd
          new = M.filterWithKey fltr old
    return
      . addConveyor o
      . C.xformToConvey
      $ (return . changer)

------------------------------------------------------------
-- INGREDIENTS
------------------------------------------------------------
replaceWithIngr :: OptDesc Opts Error
replaceWithIngr = OptDesc "" ["replace-with-ingredients"] a where
  a = Flag f
  f o = return
        . addConveyor o
        . C.trayFilterToConvey
        . C.filterToTrayFilter
        $ C.replaceWithIngr

removeIngr :: OptDesc Opts Error
removeIngr = OptDesc "" ["remove-ingredients"] a where
  a = Flag f
  f o = return
        . addConveyor o
        . C.trayFilterToConvey
        . C.filterToTrayFilter
        $ C.removeIngr

-- | Takes a list of strings and parses it into a list of FoodId.
-- Returns the error given if something goes wrong.
parseFoodIds :: (String -> R.Error) -- ^ For bad input
                -> [String]
                -> Either R.Error [F.FoodId]
parseFoodIds err ss = let
  folder str res = case res of
    (Left e) -> Left e
    (Right is) -> case fromStr str of
      Nothing -> Left (err str)
      (Just i) -> Right ((F.FoodId i):is)
   in foldr folder (Right []) ss

ingrToVolatile :: OptDesc Opts Error
ingrToVolatile = OptDesc "" ["ingredients-to-volatile"] a where
  a = Variable f
  f o as = do
    is <- parseFoodIds R.IngrToVolatileBadIdStr as
    return
      . addConveyor o
      . C.trayMToConvey
      . C.ingrToVolatile
      $ is

------------------------------------------------------------
-- REPORTING
------------------------------------------------------------
print :: OptDesc Opts Error
print = OptDesc "p" ["print"] a where
  a = Variable f
  f o as = do
    gs <- buildReportGroups as
    let x = printReportGroups (reportOpts o) gs
    return
      . addConveyor o
      . C.trayFilterToConvey
      $ C.printerToTrayFilter x

goal :: OptDesc Opts Error
goal = OptDesc "g" ["goal"] a where
  a = Double f
  f o a1 a2 = do
    let n = F.NutName . pack $ a1
    v <- case fromStr a2 of
      Nothing -> Left $ R.NonNegMixedNotValid a2
      (Just nn) -> Right $ F.NutAmt nn
    let gna = RT.GoalNameAmt n v
        newO = o { reportOpts = newRo } where
          newRo = (reportOpts o) { RT.goals = oldGoals ++ [gna] } where
            oldGoals = RT.goals . reportOpts $ o
    return newO

showAllNuts :: OptDesc Opts Error
showAllNuts = OptDesc "" ["show-all-nutrients"] a where
  a = Flag f
  f o = return newO where
    newO = o { reportOpts = newRo } where
      newRo = (reportOpts o) { RT.showAllNuts = True }

showTag :: OptDesc Opts Error
showTag = OptDesc "t" ["show-tag"] a where
  a = Single f
  f o a1 = let
    t = F.TagName . pack $ a1
    newO = o { reportOpts = newRo } where
      newRo = (reportOpts o) { RT.showTags = oldTags ++ [t] } where
        oldTags = RT.showTags . reportOpts $ o
    in return newO

showAllTags :: OptDesc Opts Error
showAllTags = OptDesc "" ["show-all-tags"] a where
  a = Flag f
  f o = return newO where
    newO = o { reportOpts = newRo } where
      newRo = (reportOpts o) { RT.showAllTags = True }

oneColumn :: OptDesc Opts Error
oneColumn = OptDesc "" ["one-column"] a where
  a = Flag f
  f o = return newO where
    newO = o { reportOpts = newRo } where
      newRo = (reportOpts o) { RT.oneColumn = True }

------------------------------------------------------------
-- SORTING
------------------------------------------------------------
-- | Zip a list into pairs. Returns Just [(a,a)] if successful;
-- returns Nothing if there is an odd number of strings.
zipPairs :: [a] -> Maybe [(a,a)]
zipPairs = r (Just []) where
  r result rest = case result of
    Nothing -> Nothing
    (Just rs) -> case rest of
      [] -> Just rs
      (_:[]) -> Nothing
      (a1:a2:as) -> case r (Just rs) as of
        (Just rs') -> Just ((a1, a2) : rs')
        Nothing -> Nothing

-- | This is strict - here for historical interest
zipL = r [] where
  r rs [] = rs
  r _ (a:[]) = error "odd number of items"
  r rs (a1:a2:as) = r (rs ++ [(a1, a2)]) as

-- | This is lazy - here for historical interest
zipR = r [] where
  r _ (a:[]) = error "odd number of items"
  r rs [] = rs
  r rs (a1:a2:as) = (a1, a2) : r rs as

-- | Generate a list of keys from a list of command-line arguments.
makeKeys :: [String] -> Either R.Error [S.Key]
makeKeys ss = case zipPairs ss of
  Nothing -> Left R.KeyOddArguments
  (Just ps) -> let
    folder (f, s) ks = case ks of
      (Left err) -> Left err
      (Right gs) -> let
        dir | f `isPrefixOf` "ascending" = Right S.Ascending
            | f `isPrefixOf` "descending" = Right S.Descending
            | otherwise = Left $ R.NoSortDirection f
        in case dir of
          (Right d) -> let
            n = F.TagName . pack $ s
            in Right ((S.Key n d) : gs)
          (Left err) -> Left err
    in foldr folder (Right []) ps

key :: OptDesc Opts Error
key = OptDesc "k" ["key"] a where
  a = Variable f
  f o as = do
    ks <- makeKeys as
    let sorter = C.sort (tagMap o) ks
        c = C.filterToConvey sorter
    return $ addConveyor o c

order :: OptDesc Opts Error
order = OptDesc "O" ["key-order"] a where
  a = Double f
  f o a1 a2 = let
    n = F.TagName . pack $ a1
    v = F.TagVal . pack $ a2
    newTagMap = S.addTag n v oldTagMap
    oldTagMap = tagMap o
    in return $ o { tagMap = newTagMap }

append :: OptDesc Opts Error
append = OptDesc "a" ["append"] a where
  a = Flag f
  f o = return
        . addConveyor o
        . C.trayFilterToConvey
        $ C.append

prepend :: OptDesc Opts Error
prepend = OptDesc "" ["prepend"] a where
  a = Flag f
  f o = return
        . addConveyor o
        . C.trayFilterToConvey
        $ C.prepend

replace :: OptDesc Opts Error
replace = OptDesc "" ["replace"] a where
  a = Flag f
  f o = return
        . addConveyor o
        . C.trayFilterToConvey
        $ C.replace

edit :: OptDesc Opts Error
edit = OptDesc "" ["edit"] a where
  a = Flag f
  f o = return 
        . addConveyor o
        . C.trayMToConvey
        $ C.edit

delete :: OptDesc Opts Error
delete = OptDesc "" ["delete"] a where
  a = Flag f
  f o = return
        . addConveyor o
        . C.trayFilterToConvey
        $ C.delete

ingrFromVolatile :: OptDesc Opts Error
ingrFromVolatile = OptDesc "" ["ingredients-from-volatile"] a where
  a = Variable f
  f o as = do
    is <- parseFoodIds R.IngrFromVolatileBadIdStr as
    let c = C.trayMToConvey (C.ingrFromVolatile is)
    return
      . addConveyor o
      $ c

-- | Canonicalizes a path handed in via a string.
canonLoadPath :: String -> T.Tray -> E.ErrorT R.Error IO P.CanonPath
canonLoadPath s t = do
  u <- case P.userPath s of
    (Left err) -> E.throwError err
    (Right good) -> return good
  let cd = T.clientCurrDir t
  P.canonLoadPath cd u
  
open :: OptDesc Opts Error
open = OptDesc "" ["open"] a where
  a = Single f
  f o a1 = let
    c t = do
      path <- canonLoadPath a1 t
      C.open path t
    in return . addConveyor o $ c

-- | Get a canonical save path.
canonSavePath :: String -> T.Tray -> E.ErrorT R.Error IO P.CanonPath
canonSavePath s t = do
  u <- case P.userPath s of
    (Left e) -> E.throwError e
    (Right g) -> return g
  let cd = T.clientCurrDir t
  P.canonSavePath cd u

appendFile :: OptDesc Opts Error
appendFile = OptDesc "" ["append-file"] a where
  a = Single f
  f o a1 = let
    c t = do
      path <- canonLoadPath a1 t
      C.appendFile path t
    in return . addConveyor o $ c

prependFile :: OptDesc Opts Error
prependFile = OptDesc "" ["prepend-file"] a where
  a = Single f
  f o a1 = let
    c t = do
      path <- canonLoadPath a1 t
      C.prependFile path t
    in return . addConveyor o $ c

close :: OptDesc Opts Error
close = OptDesc "" ["close"] a where
  a = Flag f
  f o = return . addConveyor o . C.trayFilterToConvey $ C.close

save :: OptDesc Opts Error
save = OptDesc "" ["save"] a where
  a = Flag f
  f o = return . addConveyor o $ C.save

saveAs :: OptDesc Opts Error
saveAs = OptDesc "" ["save-as"] a where
  a = Single f
  f o a1 = let
    c t = do
      path <- canonSavePath a1 t
      C.saveAs path t
    in return . addConveyor o $ c

quit :: OptDesc Opts Error
quit = OptDesc "" ["quit"] a where
  a = Flag f
  f o = return . addConveyor o . C.trayFilterToConvey $ C.quit

------------------------------------------------------------
-- HELP
------------------------------------------------------------
-- | Generic help option maker.
helpOpt :: String  -- ^ Long option string
           -> (RT.ReportOpts -> T.Tray -> T.Tray) -- ^ Tray filter
           -> OptDesc Opts err
helpOpt s g = OptDesc "" [s] a where
  a = Flag f
  f o = return
        . addConveyor o
        . C.trayFilterToConvey
        . g
        . reportOpts
        $ o
help :: OptDesc Opts Error
help = helpOpt "help" C.help

version :: OptDesc Opts Error
version = helpOpt "version" C.version

copyright :: OptDesc Opts Error
copyright = helpOpt "copyright" C.copyright

