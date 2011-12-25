module Pantry.Error where

import qualified Control.Monad.Error as E
import Pantry.Food (UnitName, FoodId, SetQtyByNutFailure,
                    Food)
import qualified Pantry.Food as F
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
           | IngrFromVolatileBadIdStr String
           | AddNutError Food
           | NoSortDirection String
           | KeyOddArguments
           | NonOptionArguments [String]

instance E.Error Error where
  strMsg = Other

instance O.ParseErr Error where
  store = ParseError

showError :: Error -> X.Text
showError e = case e of
  (NotExactlyOneMatchingUnit us) ->
    message b ls e where
      b = "not exactly one matching unit"
      ls = [ "You entered a pattern to change the unit of a food,"
           , "but there was more than one unit matching the"
           , "pattern. Matching units:"
           ] ++ map (X.unpack . F.unUnitName) us

-- | Format a standard error message. The first line will say "pantry:
-- error:" and give a one-line summary of the message. Following lines
-- will include more details about the error. The last will state the
-- pantry error number.
message :: String -- ^ Brief description
           -> [String] -- ^ More lines
           -> Error -- ^ The error, to pull out the error code
           -> X.Text
message b ls e = X.unlines $ [f] ++ m ++ [l] where
  f = X.pack $ "pantry: error: " ++ b
  m = map X.pack ls
  l = X.pack $ "(Pantry error number " ++ show (errorCode e) ++ ")"

errorCode :: Error -> Word8
errorCode e = case e of
  (NotExactlyOneMatchingUnit {})                            -> 1
  (AddNutToZeroQty {})                                      -> 2
  (RegexComp {})                                            -> 3
  (NoReportMatch {})                                        -> 4
  (Other {})                                                -> 5
  (MoveIdNotFound {})                                       -> 6
  (MultipleMoveIdMatches {})                                -> 7
  (MultipleEditIdMatches {})                                -> 8
  (MoveStartNotFound {})                                    -> 9
  (CanonicalizeError {})                                    -> 10
  (FileSaveError {})                                        -> 11
  (FileReadError {})                                        -> 12
  (FileDecodeError {})                                      -> 13
  (NotPantryFile {})                                        -> 14
  (WrongFileVersion {})                                     -> 15
  (NoSaveFilename {})                                       -> 16
  (UndoTooBig {})                                           -> 17
  (EmptyFilePath {})                                        -> 18
  (FindLoadFileError {})                                    -> 19
  (FindSaveDirError {})                                     -> 20
  (IngrToVolatileLookup {})                                 -> 21
  (IngrFromVolatileNotFound {})                             -> 22
  (ParseError {})                                           -> 23
  (IDsNotFound {})                                          -> 24
  (IDStringNotValid {})                                     -> 25
  (NonNegIntegerStringNotValid {})                          -> 26
  (NoMoveIDsGiven {})                                       -> 27
  (OneMoveIDGiven {})                                       -> 28
  (NonNegMixedNotValid {})                                  -> 29
  (PosMixedNotValid {})                                     -> 30
  (BoundedPercentNotValid {})                               -> 31
  (QByNutFail {})                                           -> 32
  (IngrToVolatileBadIdStr {})                               -> 33
  (IngrFromVolatileBadIdStr {})                             -> 34
  (AddNutError {})                                          -> 35
  (NoSortDirection {})                                      -> 36
  (KeyOddArguments {})                                      -> 37
  (NonOptionArguments {})                                   -> 38
