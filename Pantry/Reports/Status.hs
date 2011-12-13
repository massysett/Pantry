module Pantry.Reports.Status(status) where

import qualified Pantry.Tray as T
import qualified Pantry.Bag as B
import qualified Data.Text as X

status :: T.Tray -> X.Text
status t = X.unlines ls where
  ls = [filename, unsaved, count, undos, nextId]
  filename = label "Filename" $ case T.filename t of
    Nothing -> "(no filename)"
    (Just (B.Filename f)) -> f
  unsaved = label "Unsaved" $ case B.unUnsaved . T.unsaved $ t of
    True -> "yes"
    False -> "no"
  count = label "Number of foods"
          (show . length . B.unBuffer . T.buffer $ t)
  undos = label "Number of undo levels"
          (show . length . B.unUndos . T.undos $ t)
  nextId = label "Next ID" (show . T.nextId $ t)
    
label :: String -> String -> X.Text
label l t = (X.pack l) `X.append` (X.pack ": ") `X.append` (X.pack t)
