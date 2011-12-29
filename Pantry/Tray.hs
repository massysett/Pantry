{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pantry.Tray where

import Pantry.Food(Food, oneFoodId)
import qualified Data.DList as DL
import qualified Data.Text as X
import Pantry.Paths ( CanonPath, ClientDir )
import Control.DeepSeq ( NFData(rnf), deepseq)

import qualified Pantry.Bag as Bag

newtype Volatile = Volatile { unVolatile :: [Food] }
                   deriving NFData

newtype Output = Output { unOutput :: DL.DList X.Text }
data Done = Done | NotDone

data Tray = Tray { nextId :: Bag.NextId
                 , filename :: Maybe CanonPath
                 , unsaved :: Bag.Unsaved
                 , buffer :: Bag.Buffer
                 , undos :: Bag.Undos
                 , volatile :: Volatile
                 , done :: Done
                 , output :: Output
                 , clientCurrDir :: ClientDir }

compact :: Tray -> ()
compact t =
  nextId t `deepseq`
  filename t `deepseq`
  buffer t `deepseq`
  undos t `deepseq`
  volatile t `deepseq`
  clientCurrDir t `deepseq`
  ()

blankTray :: ClientDir -> Tray
blankTray c = Tray { nextId = Bag.NextId $ oneFoodId
                   , filename = Nothing
                   , unsaved = Bag.Unsaved False
                   , buffer = Bag.Buffer []
                   , undos = Bag.Undos []
                   , volatile = Volatile []
                   , done = NotDone
                   , output = Output DL.empty
                   , clientCurrDir = c }

bagToTray :: Bag.Bag -> ClientDir -> Tray
bagToTray b d = Tray { nextId = Bag.nextId b
                     , filename = Bag.filename b
                     , unsaved = Bag.unsaved b
                     , buffer = Bag.buffer b
                     , undos = Bag.undos b
                     , volatile = v
                     , done = NotDone
                     , output = o
                     , clientCurrDir = d } where
  v = Volatile . Bag.unBuffer . Bag.buffer $ b
  o = Output (DL.empty)

trayToBag :: Tray -> Maybe Bag.Bag
trayToBag t = case done t of
  NotDone -> Just Bag.Bag { Bag.nextId = nextId t
                          , Bag.filename = filename t
                          , Bag.unsaved = unsaved t
                          , Bag.buffer = buffer t
                          , Bag.undos = undos t }
  Done -> Nothing


