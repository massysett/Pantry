module Pantry.Parser (getConveyor) where

import qualified Pantry.Tray as T
import qualified Control.Monad.Error as E
import Pantry.Error(Error)
import qualified Pantry.Error as R
import Pantry.Radio.Messages ( Request )
import System.Console.OptParse.OptParse (
  OptDesc(OptDesc), ArgDesc(Flag, Single, Double, Variable))
import qualified Pantry.Matchers as M
import Data.Text ( Text, pack )
import qualified Pantry.Conveyor as C
import qualified Pantry.Reports.Types as RT
import qualified Pantry.Food as F
import Control.Monad ((>=>), mapM)
import Pantry.Types ( fromStr )

getConveyor :: Request
               -> T.Tray
               -> E.ErrorT Error IO T.Tray
getConveyor = undefined

data Opts = Opts {
  sensitive :: M.CaseSensitive,
  invert :: Bool,
  matcher :: String -> Either Error (Text -> Bool),
  conveyor :: T.Tray -> E.ErrorT Error IO T.Tray,
  reportOpts :: RT.ReportOpts }

ignoreCase = OptDesc "i" ["ignore-case"] a where
  a = Flag (\o -> return $ o { sensitive = M.CaseSensitive False })

caseSensitive = OptDesc "" ["case-sensitive"] a where
  a = Flag (\o -> return $ o { sensitive = M.CaseSensitive True })

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
                 (raiseMatcher (M.within (sensitive o)))

posix = OptDesc "" ["posix"] a where
  a = Flag f
  f o = return $ o { matcher = new } where
    new = flipCase (invert o) (M.tdfa (sensitive o))

pcre = OptDesc "" ["pcre"] a where
  a = Flag f
  f o = return $ o { matcher = new } where
    new = flipCase (invert o) (M.pcre (sensitive o))

exact = OptDesc "" ["exact"] a where
  a = Flag f
  f o = return $ o { matcher = new } where
    new = flipCase (invert o)
          (raiseMatcher (M.exact (sensitive o)))

raiseMatcher ::
  (String -> Text -> Bool)
  -> String -> Either Error (Text -> Bool)
raiseMatcher f s = Right $ f s

-- Filtering
find = OptDesc "f" ["find"] a where
  a = Double f
  f o a1 a2 = do
    m <- (matcher o) a2    
    let p = F.foodMatch (F.Name . pack $ a1) m
        c = C.trayFilterToConvey .
            C.filterToTrayFilter .
            C.predToFilter $ p
        newO = o { conveyor = conveyor o >=> c }
    return newO

strToId :: String -> Either R.Error F.FoodId
strToId s = case fromStr s of
  Nothing -> Left $ R.IDStringNotValid s
  (Just i) -> Right $ F.FoodId i

findIds = OptDesc "" ["id"] a where
  a = Variable f
  f o as = do
    is <- mapM strToId as
    let newO = o { conveyor = conveyor o >=> c }
        c = C.trayMToConvey . C.filterMToTrayM $ volatileFilter
        volatileFilter = C.findIds is
    return newO

