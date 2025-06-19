module Options.Runtime (defaultRun, RunOptions (..)) where
-- import Data.Int (Int)

import Data.Text (Text)



newtype RunOptions = RunOptions {
    debug :: Int
  }
  deriving (Show)

defaultRun :: RunOptions
defaultRun =
  RunOptions {
    debug = 0
   -- HERE: Set default value for additional runtime parameters:  , root = "/tmp"
  }
