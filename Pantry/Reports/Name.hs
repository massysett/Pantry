module Pantry.Reports.Name (name) where

import qualified Pantry.Food as F
import Data.Text(snoc, pack, Text)
import qualified Data.Map as M

name :: F.Food -> Text
name f = snoc n '\n' where
  n = case M.lookup (F.TagName . pack $ "name") (F.getTags f) of
    Nothing -> pack "(No name)"
    (Just (F.TagVal v)) -> v

