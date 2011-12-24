module Pantry.Error where

import qualified Control.Monad.Error as E
import Pantry.Food (UnitName, FoodId, SetQtyByNutFailure,
                    Food)
import Control.Exception ( IOException )
import qualified Data.Text as X
import Data.Word ( Word8 )
import Pantry.Types (NonNegInteger)
import qualified System.Console.OptParse.OptParse as O

data Error = NotExactlyOneMatchingUnit [UnitName]
           | AddNutToZeroQty
           | RegexComp String
           | NoReportMatch String [String]
           | Other String
           | MoveIdNotFound FoodId
           | MultipleMoveIdMatches FoodId
           | MultipleEditIdMatches FoodId
           | MoveStartNotFound FoodId
           | CanonicalizeError IOException
           | FileSaveError IOException
           | FileReadError IOException
           | FileDecodeError String
           | NotPantryFile
           | WrongFileVersion
           | NoSaveFilename
           | UndoTooBig NonNegInteger Integer
           | EmptyFilePath
           | FindLoadFileError IOException
           | FindSaveDirError IOException
           | IngrToVolatileLookup [FoodId]
           | IngrFromVolatileNotFound [FoodId]
           | ParseError String
           | IDsNotFound [FoodId]
           | IDStringNotValid String
           | NonNegIntegerStringNotValid String
           | NoMoveIDsGiven
           | OneMoveIDGiven
           | NonNegMixedNotValid String
           | PosMixedNotValid String
           | BoundedPercentNotValid String
           | QByNutFail SetQtyByNutFailure
           | IngrToVolatileBadIdStr String
           | AddNutError Food

instance E.Error Error where
  strMsg = Other

instance O.ParseErr Error where
  store = ParseError

showError :: Error -> X.Text
showError = undefined

errorCode :: Error -> Word8
errorCode = undefined
