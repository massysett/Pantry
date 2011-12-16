{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | File path manipulations.
module Pantry.Paths where

import qualified System.Directory as D
import qualified System.FilePath as F
import System.FilePath ((</>))
import qualified Pantry.Error as E
import qualified Control.Monad.Error as Err
import Data.Serialize (Serialize)
import qualified Control.Exception as Ex
import Control.Monad.Trans ( lift )

-- | A path the user entered on the command line. Maybe it is perfect,
-- or maybe it is flawed; might be relative or absolute.
newtype UserPath = UserPath { unUserPath :: FilePath }

-- | A path that has been canonicalized. This might mean that all
-- components exist, or it might mean that all components but the last
-- exist. Either way, it means that the path begins with a leading
-- slash.
newtype CanonPath = CanonPath { unCanonPath :: FilePath }

-- | An absolute path
newtype AbsPath = AbsPath { unAbsPath :: FilePath }

-- | The current directory of the client. Used to make UserPaths
-- absolute. This is always absolute.
newtype ClientDir = ClientDir { unClientDir :: FilePath }
                  deriving Serialize

canonLoadPath :: ClientDir
                 -> UserPath
                 -> Err.ErrorT E.Error IO CanonPath
canonLoadPath _ (UserPath []) = Err.throwError E.EmptyFilePath
canonLoadPath (ClientDir c) (UserPath u) = let
  eiToCanon ei = case ei of
    (Left err) -> Err.throwError $ E.FindLoadFileError err
    (Right good) -> return $ CanonPath good
  toCanon = case u of
    ('/':_) -> u
    _ -> c </> u
  in lift (Ex.try (D.canonicalizePath toCanon)) >>= eiToCanon

canonSavePath :: ClientDir
                 -> UserPath
                 -> Err.ErrorT E.Error IO CanonPath
canonSavePath = undefined

