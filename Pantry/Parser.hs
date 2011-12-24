module Pantry.Parser (getConveyor) where

import qualified Pantry.Tray as T
import qualified Control.Monad.Error as E
import Pantry.Error(Error)
import qualified Pantry.Error as R
import Pantry.Radio.Messages ( Request )
import System.Console.OptParse.OptParse (
  OptDesc(OptDesc), ArgDesc(Flag, Single, Double, Variable))
import qualified Data.Map as M
import qualified Pantry.Matchers as Matchers
import Data.Text ( Text, pack )
import qualified Pantry.Conveyor as C
import qualified Pantry.Reports.Types as RT
import qualified Pantry.Food as F
import Control.Monad ((>=>), mapM)
import Pantry.Types ( fromStr, NonNegInteger, NonNegMixed )

getConveyor :: Request
               -> T.Tray
               -> E.ErrorT Error IO T.Tray
getConveyor = undefined

data Opts = Opts {
  sensitive :: Matchers.CaseSensitive,
  invert :: Bool,
  matcher :: String -> Either Error (Text -> Bool),
  conveyor :: T.Tray -> E.ErrorT Error IO T.Tray,
  reportOpts :: RT.ReportOpts }

ignoreCase = OptDesc "i" ["ignore-case"] a where
  a = Flag (\o -> return $ o { sensitive = Matchers.CaseSensitive False })

caseSensitive = OptDesc "" ["case-sensitive"] a where
  a = Flag (\o -> return $ o { sensitive = Matchers.CaseSensitive True })

invertOpt = OptDesc "v" ["invert"] a where
  a = Flag (\o -> return $ o { invert = True })

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

within = OptDesc "" ["within"] a where
  a = Flag f
  f o = return $ o { matcher = newMatcher } where
    newMatcher = flipCase (invert o)
                 (raiseMatcher (Matchers.within (sensitive o)))

posix = OptDesc "" ["posix"] a where
  a = Flag f
  f o = return $ o { matcher = new } where
    new = flipCase (invert o) (Matchers.tdfa (sensitive o))

pcre = OptDesc "" ["pcre"] a where
  a = Flag f
  f o = return $ o { matcher = new } where
    new = flipCase (invert o) (Matchers.pcre (sensitive o))

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

findIds = OptDesc "" ["id"] a where
  a = Variable f
  f o as = do
    is <- mapM strToId as
    let newO = addConveyor o c
        c = C.trayMToConvey . C.filterMToTrayM $ volatileFilter
        volatileFilter = C.findIds is
    return newO

clear = OptDesc "" ["clear"] a where
  a = Flag f
  f o = return $ addConveyor o c where
    c = C.newVolatileToConvey C.clear

recopy = OptDesc "" ["recopy"] a where
  a = Flag f
  f o = return $ addConveyor o c where
    c = C.trayFilterToConvey C.recopy

strToNonNegInteger :: String -> Either R.Error NonNegInteger
strToNonNegInteger s = case fromStr s of
  Nothing -> Left (R.NonNegIntegerStringNotValid s)
  (Just i) -> Right i

head = OptDesc "" ["head"] a where
  a = Single f
  f o a1 = do
    i <- strToNonNegInteger a1
    return $ C.filterToConvey (C.head i)

tail = OptDesc "" ["tail"] a where
  a = Single f
  f o a1 = do
    i <- strToNonNegInteger a1
    return $ C.filterToConvey (C.tail i)

create = OptDesc "" ["create"] a where
  a = Flag f
  f o = return $ C.filterToConvey C.create

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

setCurrUnit = OptDesc "u" ["match-unit"] a where
  a = Single f
  f o a1 = matcher o a1 >>= \m ->
    let changeWithErr fd = case F.changeCurrUnit m fd of
          (Right good) -> Right good
          (Left err) -> Left (R.NotExactlyOneMatchingUnit err)
        c = C.xformToConvey changeWithErr
    in return $ addConveyor o c

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

changePctRefuse = OptDesc "" ["change-percent-refuse"] a where
  a = Single f
  f o a1 = case fromStr a1 of
    Nothing -> Left (R.BoundedPercentNotValid a1)
    (Just n) ->
      let xform = F.setPctRefuse (F.PctRefuse n)
          c = C.xformToConvey (return . xform)
      in return $ addConveyor o c

changeYield = OptDesc "" ["change-yield"] a where
  a = Single f
  f o a1 = case fromStr a1 of
    Nothing -> Left (R.NonNegMixedNotValid a1)
    (Just n) -> return $ addConveyor o c where
      c = C.xformToConvey (return . setYield)
      setYield fd = F.setYield y fd where
        y = F.ExplicitYield . F.PosMixedGrams $ n

removeYield = OptDesc "" ["remove-yield"] a where
  a = Flag f
  f o = return $ addConveyor o c where
    c = C.xformToConvey (return . setYield)
    setYield fd = F.setYield F.AutoYield fd

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

refuse = OptDesc "r" ["refuse"] a where
  a = Flag f
  f o = return
        . addConveyor o
        . C.xformToConvey
        $ (return . F.minusPctRefuse)

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

renameNut = OptDesc "" ["rename-nutrient"] a where
  a = Double f
  f o a1 a2 = do
    m <- matcher o a1
    let n = F.NutName . pack $ a2
    return
      . addConveyor o
      . C.xformToConvey
      $ (return . F.renameNuts m n)

deleteNut = OptDesc "" ["delete-nutrients"] a where
  a = Single f
  f o a1 = do
    m <- matcher o a1
    return
      . addConveyor o
      . C.xformToConvey
      $ (return . F.deleteNuts m )

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
