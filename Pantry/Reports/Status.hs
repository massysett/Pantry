module Pantry.Reports.Status(status) where

import qualified Pantry.Tray as T
import qualified Pantry.Bag as B
import qualified Data.Text as X
import Pantry.Exact ( exact )

status :: T.Tray -> X.Text
status t = X.unlines ls where
  ls = [filename, unsaved, count, undos, nextId]
  filename = label "Filename" $ case T.filename t of
    Nothing -> X.pack "(no filename)"
    (Just f) -> exact f
  unsaved = label "Unsaved" $ case B.unUnsaved . T.unsaved $ t of
    True -> X.pack "yes"
    False -> X.pack "no"
  count = label "Number of foods"
          (exact . length . B.unBuffer . T.buffer $ t)
  undos = label "Number of undo levels"
          (exact . length . B.unUndos . T.undos $ t)
  nextId = label "Next ID" (exact . T.nextId $ t)
    
label :: String -> X.Text -> X.Text
label l t = (X.pack l) `X.append` (X.pack ": ") `X.append` t
