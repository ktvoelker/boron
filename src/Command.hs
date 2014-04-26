
module Command where

import qualified Data.Text as T

data Command =
  BuildCommand T.Text BuildCommand
  deriving (Eq, Show)

data BuildCommand = StartBuild | StopBuild | Poll
  deriving (Eq, Show)

