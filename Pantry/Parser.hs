module Pantry.Parser where

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
import qualified Pantry.Conveyor as C
import qualified Pantry.Reports.Types as RT
import qualified Pantry.Food as F
import Control.Monad ((>=>))
import Pantry.Types ( fromStr, NonNegInteger, NonNegMixed )
import Pantry.Reports ( buildReportGroups, printReportGroups )
import qualified Pantry.Sorter as S
import qualified Pantry.Paths as P
import Control.Monad.Trans ( liftIO )
import Data.Text ( Text, pack, singleton, isPrefixOf, unpack )
import System.Console.MultiArg.Prim
import System.Console.MultiArg.Combinator
import System.Console.MultiArg.Option
import Data.Set ( Set )
import Data.Maybe ( catMaybes )
import Control.Monad.Exception.Synchronous
  ( Exceptional (Success, Exception))

data Opts = Opts {
  sensitive :: Matchers.CaseSensitive,
  invert :: Bool,
  matcher :: Text -> Either Error (Text -> Bool),
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

type PP = (String, Set LongOpt -> ParserSE Opts Error ())

noArg :: Maybe Char
         -> String
         -> Set LongOpt
         -> ParserSE Opts Error ()
noArg mc s los = let
  optL = do
    let lo = makeLongOpt . pack $ s
    (_, _, mt) <- matchApproxLongOpt lo los
    case mt of
      Nothing -> return ()
      (Just l) -> zero $ R.LongOptDoesNotTakeArgument lo
  in case mc of
    Nothing -> optL
    (Just c) -> optL <|> (shortNoArg (makeShortOpt c) >> return ())

oneArg :: Maybe Char
          -> String
          -> Set LongOpt
          -> ParserSE Opts Error Text
oneArg mc s los = let
  optL = do
    let lo = makeLongOpt . pack $ s
    (_, _, mt) <- matchApproxLongOpt lo los
    case mt of
      (Just t) -> return t
      Nothing -> nextArg
  in case mc of
    Nothing -> optL
    (Just c) -> optL <|> do
      let so = makeShortOpt c
      (_, t) <- shortOneArg so
      return t

twoArg :: Maybe Char
          -> String
          -> Set LongOpt
          -> ParserSE Opts Error (Text, Text)
twoArg mc s los = let
  optL = do
    let lo = makeLongOpt . pack $ s
    (_, _, mt) <- matchApproxLongOpt lo los
    case mt of
      (Just a1) -> do
        a2 <- nextArg
        return (a1, a2)
      Nothing -> do
        a1 <- nextArg
        a2 <- nextArg
        return (a1, a2)
  in case mc of
    Nothing -> optL
    (Just c) -> optL <|> do
      let so = makeShortOpt c
      (_, a1, a2) <- shortTwoArg so
      return (a1, a2)

variableArg :: Maybe Char
          -> String
          -> Set LongOpt
          -> ParserSE Opts Error ([Text])
variableArg mc s los = let
  optL = do
    let lo = makeLongOpt . pack $ s
    (_, _, mt) <- matchApproxLongOpt lo los
    case mt of
      (Just a1) -> do
        a2 <- many nonOptionPosArg
        return (a1 : a2)
      Nothing -> do
        a1 <- many nonOptionPosArg
        return a1
  in case mc of
    Nothing -> optL
    (Just c) -> optL <|> do
      let so = makeShortOpt c
      (_, as) <- shortVariableArg so
      return as

ignoreCase :: PP
ignoreCase = (o, f) where
  o = "ignore-case"
  f set = do
    noArg (Just 'i') o set
    modifySt (\s -> s { sensitive = Matchers.CaseSensitive False })

caseSensitive :: PP
caseSensitive = (o, f) where
  o = "case-sensitive"
  f set = do
    noArg Nothing o set
    modifySt (\s -> s { sensitive = Matchers.CaseSensitive True })

invertOpt :: PP
invertOpt = (o, f) where
  o = "invert"
  f set = do
    noArg (Just 'v') o set
    modifySt (\s -> s { invert = True })

noInvert :: PP
noInvert = (o, f) where
  o = "no-invert"
  f set = do
    noArg Nothing o set
    modifySt (\s -> s { invert = False })

flipCase :: 
  Bool  -- ^ Invert matching behavior?
  -> (Text -> Either Error (Text -> Bool))
  -> Text -> Either Error (Text -> Bool)
flipCase b f s = case b of
  True -> do
    m <- f s
    return (\t -> not (m t))
  False -> f s

within :: PP
within = (o, f) where
  o = "within"
  f set = do
    noArg Nothing o set
    s <- getSt
    let newSt = s { matcher = newMatcher }
        newMatcher = flipCase (invert s)
                     (raiseMatcher (Matchers.within (sensitive s)))
    putSt newSt

posix :: PP
posix = (o, f) where
  o = "posix"
  f set = do
    noArg Nothing o set
    s <- getSt
    let newSt = s { matcher = newMatcher }
        newMatcher = flipCase (invert s)
                     (Matchers.tdfa (sensitive s))
    putSt newSt

pcre :: PP
pcre = (o, f) where
  o = "pcre"
  f set = do
    noArg Nothing o set
    s <- getSt
    let newSt = s { matcher = newMatcher }
        newMatcher = flipCase (invert s)
                     (Matchers.pcre (sensitive s))
    putSt newSt

exact :: PP
exact = (o, f) where
  o = "exact"
  f set = do
    noArg Nothing o set
    s <- getSt
    let newSt = s { matcher = newMatcher }
        newMatcher = flipCase (invert s)
                     (raiseMatcher (Matchers.exact (sensitive s)))
    putSt newSt

raiseMatcher ::
  (Text -> Text -> Bool)
  -> Text -> Either Error (Text -> Bool)
raiseMatcher f s = Right $ f s
addConveyor :: (T.Tray -> E.ErrorT Error IO T.Tray)
               -> ParserSE Opts Error ()
addConveyor f = do
  old <- getSt
  putSt old { conveyor = conveyor old >=> f }

find :: PP
find = (o, f) where
  o = "find"
  f set = do
    (a1, a2) <- twoArg (Just 'f') o set
    o <- getSt
    m <- case (matcher o) a2 of
      (Left e) -> zero e
      (Right g) -> return g
    let p = F.foodMatch (F.TagName a1) m
        c = C.trayFilterToConvey .
            C.filterToTrayFilter .
            C.predToFilter $ p
    addConveyor c

strToId :: Text -> ParserSE Opts Error F.FoodId
strToId s = case fromStr s of
  Nothing -> zero $ R.IDStringNotValid s
  (Just i) -> return $ F.FoodId i


findIds :: PP
findIds = (o, f) where
  o = "id"
  f set = do
    as <- variableArg Nothing o set
    is <- mapM strToId as
    let c = C.trayMToConvey . C.filterMToTrayM $ volatileFilter
        volatileFilter = C.findIds is
    addConveyor c

clear :: PP
clear = (o, f) where
  o = "clear"
  f set = do
    noArg Nothing o set
    addConveyor (C.newVolatileToConvey C.clear)

recopy :: PP
recopy = (o, f) where
  o = "recopy"
  f set = do
    noArg Nothing o set
    addConveyor $ C.trayFilterToConvey C.recopy

strToNonNegInteger :: Text -> ParserSE Opts Error NonNegInteger
strToNonNegInteger s = case fromStr s of
  Nothing -> zero (R.NonNegIntegerStringNotValid s)
  (Just i) -> return i

headOpt :: PP
headOpt = (o, f) where
  o = "head"
  f set =
    oneArg Nothing o set >>=
    strToNonNegInteger >>= 
    addConveyor . C.filterToConvey . C.head

tailOpt :: PP
tailOpt = (o, f) where
  o = "tail"
  f set =
    oneArg Nothing o set >>=
    strToNonNegInteger >>= 
    addConveyor . C.filterToConvey . C.tail

create :: PP
create = (o, f) where
  o = "create"
  f set =
    noArg Nothing o set
    >> (addConveyor . C.filterToConvey $ C.create)

move :: PP
move = (o, f) where
  o = "move"
  f set = do
    as <- variableArg Nothing o set
    case as of
      [] -> zero R.NoMoveIDsGiven
      (_:[]) -> zero R.OneMoveIDGiven
      (as1:ass) -> do
        first <- case as1 == singleton 'f' || as1 == singleton 'F' of
          True -> return C.Beginning
          False ->
            strToNonNegInteger as1
            >>= (return . C.After . F.FoodId)       
        rest <-
          mapM strToNonNegInteger ass
          >>= (return . map F.FoodId)
        let volatileChanger = C.move first rest
            c = C.trayMToConvey . C.filterMToTrayM $ volatileChanger
        addConveyor c

undo :: PP
undo = (o, f) where
  o = "undo"
  f set =
    oneArg Nothing o set
    >>= strToNonNegInteger
    >>= (addConveyor . C.trayMToConvey . C.undo)

changeTag :: PP
changeTag = (o, f) where
  o = "change-tag"
  f set = do
    (a1, a2) <- twoArg (Just 'c') o set
    let n = F.TagName a1
        v = F.TagVal a2
        ct fd = F.setTags newTags fd where
          oldTags = F.getTags fd
          newTags = M.insert n v oldTags
        c = C.xformToConvey (return . ct)
    addConveyor c

parseEither :: Either e a
               -> ParserSE s e a
parseEither e = case e of
  (Left err) -> zero err
  (Right g) -> return g


deleteTag :: PP
deleteTag = (o, f) where
  o = "delete-tag"
  f set = do
    s <- getSt
    a <- oneArg Nothing o set
    m <- parseEither . matcher s $ a
    let xformer fd = F.setTags new fd where
          old = F.getTags fd
          p ((F.TagName n), _) = not . m $ n
          new = M.fromList
                . filter p
                . M.assocs
                $ old
        c = C.xformToConvey (return . xformer)
    addConveyor c

matchUnit :: PP
matchUnit = (o, f) where
  o = "match-unit"
  f set = do
    s <- getSt
    a <- oneArg (Just 'u') o set
    m <- parseEither (matcher s a)
    let changeWithErr fd = case F.changeCurrUnit m fd of
          (Right good) -> Right good
          (Left err) -> Left (R.NotExactlyOneMatchingUnit err)
        c = C.xformToConvey changeWithErr
    addConveyor c

setCurrUnit :: PP
setCurrUnit = (o, f) where
  o = "set-unit"
  f set = do
    (a1, a2) <- twoArg Nothing o set
    let n = F.UnitName a1
    v <- case fromStr a2 of
      Nothing -> zero $ R.PosMixedNotValid a2
      (Just p) -> return p
    let xform = F.setCurrUnit (F.CurrUnit n v)
        c = C.xformToConvey (return . xform)
    addConveyor c

changeQty :: PP
changeQty = (o, f) where
  o = "change-quantity"
  f set = do
    a <- oneArg Nothing o set
    nnm <- case fromStr a of
      Nothing -> zero $ R.NonNegMixedNotValid a
      (Just n) -> return n
    let q = F.Qty (Right (nnm :: NonNegMixed))
        xform fd = F.setQty q fd
        c = C.xformToConvey (return . xform)
    addConveyor c

changePctRefuse :: PP
changePctRefuse = (o, f) where
  o = "change-pct-refuse"
  f set = do
    a <- oneArg Nothing o set
    pc <- case fromStr a of
      Nothing -> zero $ R.BoundedPercentNotValid a
      (Just n) -> return n
    let xform = F.setPctRefuse (F.PctRefuse pc)
        c = C.xformToConvey (return . xform)
    addConveyor c

changeYield :: PP
changeYield = (o, f) where
  o = "change-yield"
  f set = do
    a <- oneArg Nothing o set
    yl <- case fromStr a of
      Nothing -> zero $ R.NonNegMixedNotValid a
      (Just n) -> return n
    let c = C.xformToConvey (return . setYield)
        setYield fd = F.setYield y fd where
          y = F.ExplicitYield . F.PosMixedGrams $ yl
    addConveyor c

removeYield :: PP
removeYield = (o, f) where
  o = "remove-yield"
  f set = do
    noArg Nothing o set
    let c = C.xformToConvey (return . setYield)
        setYield fd = F.setYield F.AutoYield fd
    addConveyor c

byNutrient :: PP
byNutrient = (o, f) where
  o = "by-nutrient"
  f set = do
    (a1, a2) <- twoArg Nothing o set
    s <- getSt
    m <- parseEither (matcher s a1)
    q <- case fromStr a2 of
      Nothing -> zero (R.NonNegMixedNotValid a2)
      (Just g) -> return g
    let setQ fn = case F.setQtyByNut m q fn of
          (Left err) -> Left (R.QByNutFail err)
          (Right good) -> Right good
        c = C.xformToConvey setQ
    addConveyor c

refuse :: PP
refuse = (o, f) where
  o = "refuse"
  f set = do
    noArg (Just 'r') o set
    let c = C.xformToConvey (return . F.minusPctRefuse)
    addConveyor c

addNut :: PP
addNut = (o, f) where
  o = "add-nutrient"
  f set = do
    (a1, a2) <- twoArg Nothing o set
    nn <- case fromStr a2 of
      Nothing -> zero (R.NonNegMixedNotValid a2)
      (Just val) -> return val
    let n = F.NutName a1
        v = F.NutAmt nn
        adder fd = let
          old = F.getNuts fd
          new = M.insert n v old
          in case F.setNuts new fd of
            Nothing -> Left $ R.AddNutError fd
            (Just fd') -> Right fd'
        c = C.xformToConvey adder
    addConveyor c

changeNut :: PP
changeNut = (o, f) where
  o = "change-nutrient"
  f set = do
    (a1, a2) <- twoArg Nothing o set
    s <- getSt
    m <- parseEither (matcher s a1)
    q <- case fromStr a2 of
      Nothing -> zero (R.NonNegMixedNotValid a2)
      (Just nn) -> return (F.NutAmt nn)
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
    addConveyor (C.xformToConvey changer)

renameNut :: PP
renameNut = (o, f) where
  o = "rename-nutrient"
  f set = do
    (a1, a2) <- twoArg Nothing o set
    s <- getSt
    m <- parseEither $ matcher s a1
    let n = F.NutName a2
    addConveyor . C.xformToConvey $ (return . F.renameNuts m n)

deleteNut :: PP
deleteNut = (o, f) where
  o = "delete-nutrients"
  f set = do
    a <- oneArg Nothing o set
    s <- getSt
    m <- parseEither $ matcher s a
    addConveyor . C.xformToConvey $ (return . F.deleteNuts m)

addAvailUnit :: PP
addAvailUnit = (o, f) where
  o = "add-available-unit"
  f set = do
    (a1, a2) <- twoArg Nothing o set
    let n = F.UnitName a1
    v <- case fromStr a2 of
      Nothing -> zero (R.PosMixedNotValid a2)
      (Just pm) -> return . F.UnitAmt $ pm
    let adder fd = F.setUnits new fd where
          old = F.getUnits fd
          new = M.insert n v old
    addConveyor . C.xformToConvey $ (return . adder)

changeAvailUnit :: PP
changeAvailUnit = (o, f) where
  o = "change-avail-unit"
  f set = do
    (a1, a2) <- twoArg Nothing o set
    s <- getSt
    m <- parseEither $ matcher s a1
    v <- case fromStr a2 of
      Nothing -> zero (R.PosMixedNotValid a2)
      (Just pm) -> return . F.UnitAmt $ pm
    let mapfn (F.UnitName n) amt = case m n of
          True -> v
          False -> amt
        changer fd = F.setUnits new fd where
          old = F.getUnits fd
          new = M.mapWithKey mapfn old
    addConveyor . C.xformToConvey $ (return . changer)

-- TODO on all renames, fail if more than one thing is renamed
renameAvailUnit :: PP
renameAvailUnit = (o, f) where
  o = "rename-avail-unit"
  f set = do
    (a1, a2) <- twoArg Nothing o set
    s <- getSt
    m <- parseEither $ matcher s a1
    let v = F.UnitName a2
        mapfn u@(F.UnitName n) = case m n of
          True -> v
          False -> u
        changer fd = F.setUnits new fd where
          old = F.getUnits fd
          new = M.mapKeys mapfn old
    addConveyor . C.xformToConvey $ (return . changer)

deleteAvailUnit :: PP
deleteAvailUnit = (o, f) where
  o = "delete-avail-unit"
  f set = do
    a <- oneArg Nothing o set
    s <- getSt
    m <- parseEither $ matcher s a
    let fltr (F.UnitName n) _ = not . m $ n
        changer fd = F.setUnits new fd where
          old = F.getUnits fd
          new = M.filterWithKey fltr old
    addConveyor . C.xformToConvey $ (return . changer)

------------------------------------------------------------
-- INGREDIENTS
------------------------------------------------------------
replaceWithIngr :: PP
replaceWithIngr = (o, f) where
  o = "replace-with-ingredients"
  f set = do
    noArg Nothing o set
    addConveyor
      . C.trayFilterToConvey
      . C.filterToTrayFilter
      $ C.replaceWithIngr

removeIngr :: PP
removeIngr = (o, f) where 
  o = "remove-ingredients"
  f set = do
    noArg Nothing o set
    addConveyor
      . C.trayFilterToConvey
      . C.filterToTrayFilter
      $ C.removeIngr

-- | Takes a list of strings and parses it into a list of FoodId.
-- Returns the error given if something goes wrong.
parseFoodIds :: (Text -> R.Error) -- ^ For bad input
                -> [Text]
                -> Either R.Error [F.FoodId]
parseFoodIds err ss = let
  folder str res = case res of
    (Left e) -> Left e
    (Right is) -> case fromStr str of
      Nothing -> Left (err str)
      (Just i) -> Right ((F.FoodId i):is)
   in foldr folder (Right []) ss

ingrToVolatile :: PP
ingrToVolatile = (o, f) where
  o = "ingredients-to-volatile"
  f set = do
    as <- variableArg Nothing o set
    is <- parseEither $ parseFoodIds R.IngrToVolatileBadIdStr as
    addConveyor
      . C.trayMToConvey
      . C.ingrToVolatile
      $ is

------------------------------------------------------------
-- REPORTING
------------------------------------------------------------
printOpt :: PP
printOpt = (o, f) where
  o = "print"
  f set = do
    a <- variableArg (Just 'p') o set
    gs <- parseEither $ buildReportGroups a
    s <- getSt
    let x = printReportGroups (reportOpts s) gs
    addConveyor . C.trayFilterToConvey $ C.printerToTrayFilter x

goal :: PP
goal = (o, f) where
  o = "goal"
  f set = do
    (a1, a2) <- twoArg (Just 'g') o set
    let n = F.NutName a1
    v <- case fromStr a2 of
      Nothing -> zero $ R.NonNegMixedNotValid a2
      (Just nn) -> return $ F.NutAmt nn
    s <- getSt
    let gna = RT.GoalNameAmt n v
        newS = s { reportOpts = newRo } where
          newRo = (reportOpts s) { RT.goals = oldGoals ++ [gna] } where
            oldGoals = RT.goals . reportOpts $ s
    putSt newS

showAllNuts :: PP
showAllNuts = (o, f) where
  o = "show-all-nutrients"
  f set = do
    noArg Nothing o set
    s <- getSt
    let newS = s { reportOpts = newRo }
        newRo = (reportOpts s) { RT.showAllNuts = True }
    putSt newS

showTag :: PP
showTag = (o, f) where
  o = "show-tag"
  f set = do
    a <- oneArg (Just 't') o set
    s <- getSt
    let t = F.TagName a
        newO = s { reportOpts = newRo }
        newRo = (reportOpts s) { RT.showTags = oldTags ++ [t] }
        oldTags = RT.showTags . reportOpts $ s
    putSt newO

showAllTags :: PP
showAllTags = (o, f) where
  o = "show-all-tags"
  f set = do
    noArg Nothing o set
    s <- getSt
    let newO = s { reportOpts = newRo }
        newRo = (reportOpts s) { RT.showAllTags = True }
    putSt newO

oneColumn :: PP
oneColumn = (o, f) where
  o = "one-column"
  f set = do
    noArg Nothing o set
    s <- getSt
    let newO = s { reportOpts = newRo }
        newRo = (reportOpts s) { RT.oneColumn = True }
    putSt newO

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
_zipL :: [t] -> [(t, t)]
_zipL = r [] where
  r rs [] = rs
  r _ (_:[]) = error "odd number of items"
  r rs (a1:a2:as) = r (rs ++ [(a1, a2)]) as

-- | This is lazy - here for historical interest
_zipR :: [t] -> [(t, t)]
_zipR = r [] where
  r _ (_:[]) = error "odd number of items"
  r rs [] = rs
  r rs (a1:a2:as) = (a1, a2) : r rs as

-- | Generate a list of keys from a list of command-line arguments.
makeKeys :: [Text] -> Either R.Error [S.Key]
makeKeys ss = case zipPairs ss of
  Nothing -> Left R.KeyOddArguments
  (Just ps) -> let
    folder (f, s) ks = case ks of
      (Left err) -> Left err
      (Right gs) -> let
        dir | f `isPrefixOf` (pack "ascending") = Right S.Ascending
            | f `isPrefixOf` (pack "descending") = Right S.Descending
            | otherwise = Left $ R.NoSortDirection f
        in case dir of
          (Right d) -> let
            n = F.TagName s
            in Right ((S.Key n d) : gs)
          (Left err) -> Left err
    in foldr folder (Right []) ps

key :: PP
key = (o, f) where
  o = "key"
  f set = do
    a <- variableArg (Just 'k') o set
    ks <- parseEither $ makeKeys a
    s <- getSt
    let sorter = C.sort (tagMap s) ks
        c = C.filterToConvey sorter
    addConveyor c

order :: PP
order = (o, f) where
  o = "key-order"
  f set = do
    (a1, a2) <- twoArg (Just 'k') o set
    s <- getSt
    let n = F.TagName a1
        v = F.TagVal a2
        newTagMap = S.addTag n v oldTagMap
        oldTagMap = tagMap s
    putSt s { tagMap = newTagMap }

append :: PP
append = (o, f) where
  o = "append"
  f set = do
    noArg (Just 'a') o set
    addConveyor . C.trayFilterToConvey $ C.append

prepend :: PP
prepend = (o, f) where
  o = "prepend"
  f set = do
    noArg Nothing o set
    addConveyor . C.trayFilterToConvey $ C.prepend

replace :: PP
replace = (o, f) where
  o = "replace"
  f set = do
    noArg Nothing o set
    addConveyor . C.trayFilterToConvey $ C.replace

edit :: PP
edit = (o, f) where
  o = "edit"
  f set = do
    noArg Nothing o set
    addConveyor . C.trayMToConvey $ C.edit

delete :: PP
delete = (o, f) where
  o = "delete"
  f set = do
    noArg Nothing o set
    addConveyor . C.trayFilterToConvey $ C.delete

ingrFromVolatile :: PP
ingrFromVolatile = (o, f) where 
  o = "ingredients-from-volatile"
  f set = do
    as <- variableArg Nothing o set
    is <- parseEither $ parseFoodIds R.IngrFromVolatileBadIdStr as
    let c = C.trayMToConvey (C.ingrFromVolatile is)
    addConveyor c

-- | Canonicalizes a path handed in via a string.
canonLoadPath :: String -> T.Tray -> E.ErrorT R.Error IO P.CanonPath
canonLoadPath s t = do
  u <- case P.userPath s of
    (Left err) -> E.throwError err
    (Right good) -> return good
  let cd = T.clientCurrDir t
  P.canonLoadPath cd u
  
open :: PP
open = (o, f) where
  o = "open"
  f set = do
    a <- oneArg Nothing o set
    let c t = do
          path <- canonLoadPath (unpack a) t
          C.open path t
    addConveyor c

-- | Get a canonical save path.
canonSavePath :: String -> T.Tray -> E.ErrorT R.Error IO P.CanonPath
canonSavePath s t = do
  u <- case P.userPath s of
    (Left e) -> E.throwError e
    (Right g) -> return g
  let cd = T.clientCurrDir t
  P.canonSavePath cd u

appendFileOpt :: PP
appendFileOpt = (o, f) where
  o = "append-file"
  f set = do
    a <- oneArg Nothing o set
    let c t = do
          path <- canonLoadPath (unpack a) t
          C.appendFile path t
    addConveyor c

prependFile :: PP
prependFile = (o, f) where
  o = "prepend-file"
  f set = do
    a <- oneArg Nothing o set
    let c t = do
          path <- canonLoadPath (unpack a) t
          C.prependFile path t
    addConveyor c

close :: PP
close = (o, f) where
  o = "close"
  f set = do
    noArg Nothing o set
    addConveyor . C.trayFilterToConvey $ C.close

save :: PP
save = (o, f) where
  o = "save"
  f set = do
    noArg Nothing o set
    addConveyor C.save

saveAs :: PP
saveAs = (o, f) where
  o = "save-as"
  f set = do
    a <- oneArg Nothing o set
    let c t = do
          path <- canonSavePath (unpack a) t
          C.saveAs path t
    addConveyor c

quit :: PP
quit = (o, f) where
  o = "quit"
  f set = do
    noArg Nothing o set
    addConveyor . C.trayFilterToConvey $ C.quit

------------------------------------------------------------
-- COMPACTION
------------------------------------------------------------
compact :: PP
compact = (o, f) where
  o = "compact"
  f set = do
    noArg Nothing o set
    let c t = do
          liftIO . C.compact $ t
          return t
    addConveyor c

------------------------------------------------------------
-- HELP
------------------------------------------------------------
-- | Generic help option maker.
helpOpt :: String  -- ^ Long option string
           -> (RT.ReportOpts -> T.Tray -> T.Tray) -- ^ Tray filter
           -> PP
helpOpt s g = (o, f) where
  o = s
  f set = do
    noArg Nothing o set
    s <- getSt
    addConveyor
      . C.trayFilterToConvey
      . g
      . reportOpts
      $ s

help :: PP
help = helpOpt "help" C.help

version :: PP
version = helpOpt "version" C.version

copyright :: PP
copyright = helpOpt "copyright" C.copyright

