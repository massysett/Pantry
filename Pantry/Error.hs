module Pantry.Error where

import qualified Control.Monad.Error as E
import Pantry.Food (Name, Grams, FoodId)
import Control.Exception ( IOException )
import qualified Data.Text as X
import Data.Word ( Word8 )
import Pantry.Types (NonNegInteger)

data Error = NoMatchingUnit
           | MultipleMatchingUnits [(Name, Grams)]
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


instance E.Error Error where
  strMsg = Other

showError :: Error -> X.Text
showError = undefined

errorCode :: Error -> Word8
errorCode = undefined
