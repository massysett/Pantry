module Pantry.Tray where

import Pantry.Food(Food)
import qualified Data.DList as DL
import qualified Data.Text as X

import Pantry.Bag(NextId, Filename, Unsaved, Undos, Buffer)

newtype Volatile = Volatile { unVolatile :: [Food] }
newtype Output = Output { unOutput :: DL.DList X.Text }
data Done = Done | NotDone

data Tray = Tray { nextId :: NextId
                 , filename :: Maybe Filename
                 , unsaved :: Unsaved
                 , buffer :: Buffer
                 , undos :: Undos
                 , volatile :: Volatile
                 , done :: Done
                 , output :: Output }

