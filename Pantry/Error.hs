module Pantry.Error where

import qualified Control.Monad.Error as E
import qualified Pantry.Types as T
import Control.Exception ( IOException )
import qualified Data.Text as X
import Data.Word ( Word8 )

data Error = NoMatchingUnit
           | MultipleMatchingUnits [(T.Name, T.Grams)]
           | AddNutToZeroQty
           | RegexComp String
           | NoReportMatch String [String]
           | Other String
           | MoveIdNotFound T.FoodId
           | MultipleMoveIdMatches T.FoodId
           | MultipleEditIdMatches T.FoodId
           | MoveStartNotFound T.FoodId
           | CanonicalizeError IOException
           | FileSaveError IOException
           | FileReadError IOException
           | FileDecodeError String
           | NotPantryFile
           | WrongFileVersion
           | NoSaveFilename
           | UndoTooBig T.NonNegInteger Integer

instance E.Error Error where
  strMsg = Other

showError :: Error -> X.Text
showError = undefined

errorCode :: Error -> Word8
errorCode = undefined
