-- | File path manipulations.
module Pantry.Paths where

import qualified System.Directory as D
import qualified System.FilePath as F

-- | A path the user entered on the command line. Maybe it is perfect,
-- or maybe it is flawed; might be relative or absolute.
newtype UserPath = UserPath { unUserPath :: FilePath }

-- | A path that has been canonicalized. This might mean that all
-- components exist, or it might mean that all components but the last
-- exist. Either way, it means that the path begins with a leading
-- slash.
newtype CanonPath = CanonPath { unCanonPath :: FilePath }

-- | An absolute path

-- | The current directory of the client. Used to make UserPaths
-- absolute. This is always absolute.
newtype ClientDir = ClientDir { unClientDir :: FilePath }
