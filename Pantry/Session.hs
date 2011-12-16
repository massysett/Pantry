module Pantry.Session ( session ) where

import Pantry.Radio (getListener, getRequest, processBag, Listener)
import Pantry.Bag(Bag, emptyBag)
import Pantry.Parser ( getConveyor )

session :: IO ()
session = do
  l <- getListener
  sessionLoop emptyBag l

sessionLoop :: Bag
               -> Listener
               -> IO ()
sessionLoop b l = do
  r <- getRequest l
  case r of
    Nothing -> sessionLoop b l
    (Just m) -> do
      let conveyor = getConveyor m
      maybeNewBag <- processBag b conveyor
      case maybeNewBag of
        Nothing -> return ()
        (Just newBag) -> sessionLoop newBag l

