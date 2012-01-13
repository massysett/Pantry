module Pantry.Error where

import qualified Control.Monad.Error as E
import Pantry.Food (UnitName, FoodId,
                    SetQtyByNutFailure(QBNNoMatchingNut,
                                       QBNMultipleMatchingNuts,
                                       QBNNutIsZero),
                    Food)
import qualified Pantry.Food as F
import Control.Exception ( IOException )
import qualified Data.Text as X
import Data.Text ( Text, pack, unpack )
import Data.Word ( Word8 )
import Pantry.Types (NonNegInteger)
import qualified Pantry.Types as T
import Pantry.Exact ( exact )
import qualified Data.Map as M
import System.Console.MultiArg.Option ( LongOpt, unLongOpt )
import qualified System.Console.MultiArg.Error as MAE 

data Error = NotExactlyOneMatchingUnit [UnitName]
           | AddNutToZeroQty
           | RegexComp Text
           | NoReportMatch Text [Text]
           | Other Text
           | MoveIdNotFound FoodId
           | MultipleMoveIdMatches FoodId
           | MultipleEditIdMatches FoodId
           | MoveStartNotFound FoodId
           | CanonicalizeError IOException
           | FileSaveError IOException
           | FileReadError IOException
           | FileDecodeError Text
           | NotPantryFile
           | WrongFileVersion
           | NoSaveFilename
           | UndoTooBig NonNegInteger Integer
           | EmptyFilePath
           | FindLoadFileError IOException
           | FindSaveDirError IOException
           | IngrToVolatileLookup [FoodId]
           | IngrFromVolatileNotFound [FoodId]
           | ParseError MAE.Expecting MAE.Saw
           | IDsNotFound [FoodId]
           | IDStringNotValid Text
           | NonNegIntegerStringNotValid Text
           | NoMoveIDsGiven
           | OneMoveIDGiven
           | NonNegMixedNotValid Text
           | PosMixedNotValid Text
           | BoundedPercentNotValid Text
           | QByNutFail SetQtyByNutFailure
           | IngrToVolatileBadIdStr Text
           | IngrFromVolatileBadIdStr Text
           | AddNutError Food
           | NoSortDirection Text
           | KeyOddArguments
           | NonOptionArguments [Text]
           | LongOptDoesNotTakeArgument LongOpt
           deriving Show

instance E.Error Error where
  strMsg = Other . pack

instance MAE.Error Error where
  unexpected = ParseError

showError :: Error -> X.Text
showError e = case e of
  (NotExactlyOneMatchingUnit us) ->
    message b ls e Clean where
      b = "not exactly one matching unit"
      ls = [ "You entered a pattern to change the unit of a food,"
           , "but there was more than one unit matching the"
           , "pattern. Matching units:"
           ] ++ map (X.unpack . F.unUnitName) us
  
  (AddNutToZeroQty) ->
    message b ls e Clean where
      b = "attempted to add nutrient to food whose quantity is zero"
      ls = [ "This cannot be done as Pantry cannot determine how to"
           , "store the nutrient value."
           ]
  
  (RegexComp s) ->
    message b ls e Clean where
      b = "could not compute regular expression"
      ls = [ "The pattern you gave for a regular expression resulted"
           , "in a compilation error. The error message was:"
           , unpack s
           ]
  
  (NoReportMatch s ss) ->
    message b ls e Clean where
      b = "not exactly one matching report"
      ls = [ "The string you gave for a report name does not match"
           , "exactly one report name. String you gave:"
           , unpack s
           , "Matching report names:" ]
           ++ case ss of [] -> ["(none)"]
                         rs -> map unpack rs
  
  (Other s) ->
    message b ls e Dirty where
      b = "an unknown error occurred"
      ls = [ "Description of the error: " ++ unpack s
           ,  "This is a bug; please report it to"
           , "omari@smileystation.com"
           ]
  
  (MoveIdNotFound i) ->
    message b ls e Clean where
      b = "Food ID specified for move option not found"
      ls = [ "You want to move a food, but the ID you specified was"
           , "not found. ID you specified:"
           , show i
           ]
  
  (MultipleMoveIdMatches i) ->
    message b ls e Clean where
      b = "Food ID specified for move found multiple foods"
      ls = [ "You want to move a food, but the ID you specified"
           , "matched more than one food in volatile."
           , "ID you specified:"
           , show i
           ]
  
  (MultipleEditIdMatches i) ->
    message b ls e Clean where
      b = "Edit option found multiple identical IDs in volatile"
      ls = [ "The edit option requires that each Food ID in volatile"
           , "be unique, but there are multiple foods with"
           , "the same ID. ID that matches multiple foods:"
           , show i
           ]
  
  (MoveStartNotFound i) ->
    message b ls e Clean where
      b = "ID specified as basis for move option not found"
      ls = [ "The first food ID you gave for the move option"
           , "was not found. ID you specified:"
           , show i
           ]
  
  (CanonicalizeError x) ->
    message b ls e Clean where
      b = "Could not canonicalize a path"
      ls = [ "Pantry converts all paths to canonical paths before"
           , "loading or saving files. A path you gave could not be"
           , "canonicalized. Make sure the path is valid."
           , "Error given by the operating system:"
           , show x
           ]
  
  (FileSaveError x) ->
    message b ls e Dirty where
      b = "IO error occurred while saving a file to disk"
      ls = [ "Error given by the operating system:"
           , show x
           ]

  (FileReadError x) ->
    message b ls e Clean where
      b = "IO error occurred while reading a file from disk"
      ls = [ "Error given by the operating system:"
           , show x
           ]
  
  (FileDecodeError s) ->
    message b ls e Clean where
      b = "Error occurred when decoding file contents"
      ls = [ "All data was loaded from disk, but Pantry encountered"
           , "an error when decoding the contents of the file."
           , "Further description of the error:"
           , unpack s
           ]
  
  (NotPantryFile) ->
    message b [] e Clean where
      b = "File name given for opening is not a Pantry file"
  
  (WrongFileVersion) ->
    message b ls e Clean where
      b = "File name given for opening is wrong version"
      ls = [ "File name given is a Pantry file, but it is"
           , "not compatible with this version of Pantry."
           ]
  
  (NoSaveFilename) ->
    message b ls e Clean where
      b = "Save option used but there is no default filename"
      ls = [ "Try using the save-as option instead." ]
  
  (UndoTooBig nni i) ->
    message b ls e Clean where
      b = "Number given for undo option is too large"
      ls = [ "The number you gave for the undo option exceeds"
           , "the number of undo levels currently stored."
           , "Number you gave: " ++ (show . T.unNonNegInteger $ nni)
           , "Number of levels stored: " ++ show i
           ]
  
  (EmptyFilePath) ->
    message b ls e Clean where
      b = "Empty file path"
      ls = [ "You used an option that requires a file path,"
           , "but you passed an empty string."
           ]
  
  (FindLoadFileError x) ->
    message b ls e Clean where
      b = "Could not find file to load"
      ls = [ "The file name you gave to load a file could"
           , "not be found. The error given by the"
           , "operating system:"
           , show x
           ]
  
  (FindSaveDirError x) ->
    message b ls e Clean where
      b = "Could not find directory to save in"
      ls = [ "When saving to disk Pantry must first locate the"
           , "directory in which to save your file, but the"
           , "directory could not be opened. The error given"
           , "by the operating system:"
           , show x
           ]
  
  (IngrToVolatileLookup is) ->
    message b ls e Clean where
      b = "ingredients-to-volatile could not find a food"
      ls = [ "One or more of the Food IDs given for the"
           , "ingredients-to-volatile command could not be found"
           , "in the buffer. IDs that were not found:"
           ] ++ (map X.unpack . map exact $ is)
  
  (IngrFromVolatileNotFound is) ->
    message b ls e Clean where
      b = "ingredients-from-volatile could not find a food"
      ls = [ "One or more of the Food IDs given for the"
           , "ingredients-from-volatile command could not be found"
           , "in the buffer. IDs that were not found:"
           ] ++ (map X.unpack . map exact $ is)
  
  (ParseError ex s) ->
    message b ls e Clean where
      b = "Error while parsing the command line"
      ls = [ "expected to see: " ++ unpack (MAE.printExpecting ex)
           , "actually saw: " ++ unpack (MAE.printSaw s)
           ]
  
  (IDsNotFound is) ->
    message b ls e Clean where
      b = "IDs specified with the id command not found"
      ls = [ "One or more of the Food IDs specified for the"
           , "--id command not found. IDs not found:"
           ] ++ (map X.unpack . map exact $ is)
  
  (IDStringNotValid s) ->
    message b ls e Clean where
      b = "String given for ID invalid"
      ls = [ "A string you gave for a food ID is not valid."
           , "String you gave: " ++ unpack s
           ]
  
  (NonNegIntegerStringNotValid s) ->
    message b ls e Clean where
      b = "String given for a non-negative integer invalid"
      ls = [ "String you gave: " ++ unpack s
           ]
  
  (NoMoveIDsGiven) ->
    message b ls e Clean where
      b = "No food IDs were specified for the move command"
      ls = []

  (OneMoveIDGiven) ->
    message b ls e Clean where
      b = "Only one food ID was specified for the move command"
      ls = []

  (NonNegMixedNotValid s) ->
    message b ls e Clean where
      b = "Invalid text for a non-negative mixed number"
      ls = [ "Invalid text given: " ++ unpack s ]
  
  (PosMixedNotValid s) ->
    message b ls e Clean where
      b = "Invalid text for a positive mixed number"
      ls = [ "Invalid text given: " ++ unpack s ]
  
  (BoundedPercentNotValid s) ->
    message b ls e Clean where
      b = "Invalid text for percent"
      ls = [ "Invalid text given: " ++ unpack s ]
  
  (QByNutFail q) ->
    message b ls e Clean where
      b = "Could not set quantity by nutrient"
      ls = case q of
        QBNNoMatchingNut ->
          [ "No nutrient was found matching the pattern given." ]
        (QBNMultipleMatchingNuts nns) ->
          [ "Multiple nutrients were found matching the pattern given."
          , "Matching nutrients:"
          ] ++ (map X.unpack . map F.unNutName $ nns)
        (QBNNutIsZero nn) ->
          [ "Nutrient's value is zero. Nutrient name: "
            ++ (X.unpack . F.unNutName $ nn) ]
  
  (IngrToVolatileBadIdStr s) ->
    message b ls e Clean where
      b = "Invalid string given for ingredients-to-volatile"
      ls = [ "String given for ingredients-to-volatile is not valid."
           , "String given: " ++ unpack s
           ]
  
  (IngrFromVolatileBadIdStr s) ->
    message b ls e Clean where
      b = "Invalid string given for ingredients-from-volatile"
      ls = [ "String given for ingredients-from-volatile is not valid."
           , "String given: " ++ unpack s
           ]

  (AddNutError f) ->
    message b ls e Clean where
      b = "Cannot add nutrient to foods whose weight is zero"
      ls = [ "Nutrients cannot be added to foods whose weight"
           , "is zero because Pantry cannot calculate the amount"
           , "of nutrient the food has."
           , "Name of food whose quantity is zero:"
           , (let name = F.TagName . X.pack $ "name"
              in case M.lookup name . F.getTags $ f of
                Nothing -> "(no name)"
                (Just n) -> X.unpack . F.unTagVal $ n)
           , "ID of food whose quantity is zero:"
           , (X.unpack . exact . F.getFoodId $ f)
           ]
  
  (NoSortDirection s) ->
    message b ls e Clean where
      b = "No direction for sorting given"
      ls = [ "The --key option requires either \"ascending\""
             , "or \"descending\" to be specified."
             , "Text you gave: " ++ unpack s
             ]

  (KeyOddArguments) ->
    message b ls e Clean where
      b = "Odd number of arguments to the --key option given"
      ls = [ "The --key option require an even number of arguments." ]
  
  (NonOptionArguments ss) ->
    message b ls e Clean where
      b = "Non-option arguments given"
      ls = [ "All arguments on the Pantry command line must be"
           , "options or arguments to options, but you gave"
           , "non-option arguments. Non-option arguments you gave:"
           ] ++ map unpack ss

  (LongOptDoesNotTakeArgument lo) ->
    message b ls e Clean where
      b = "Option given for long argument that does not take option"
      ls = [ "You gave an argument for an option that does not take"
           , "any arguments. Long option you gave:"
           , unpack . unLongOpt $ lo ]

-- | Dirty indicates that after this error, files on disk may have
-- been changed. Clean indicates that no data on disk was changed.
data DiskState = Clean | Dirty

-- | Format a standard error message. The first line will say "pantry:
-- error:" and give a one-line summary of the message. Following lines
-- will include more details about the error. The last will state the
-- pantry error number.
message :: String -- ^ Brief description
           -> [String] -- ^ More lines
           -> Error -- ^ The error, to pull out the error code
           -> DiskState
           -> X.Text
message b ls e s = X.unlines $ [f] ++ m ++ [stateStr, l] where
  f = X.pack $ "pantry: error: " ++ b
  m = map X.pack ls
  stateStr = X.pack $ "Data in memory will remain unchanged.\n"
             ++ case s of
               Clean -> "No data on disk was changed."
               Dirty -> "Data on disk may have been changed."
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
  (LongOptDoesNotTakeArgument {})                           -> 39
