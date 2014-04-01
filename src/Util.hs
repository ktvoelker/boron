
module Util where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit
import System.IO

todo :: a
todo = error "Not implemented"

abort :: T.Text -> IO a
abort msg = do
  TIO.hPutStrLn stderr msg
  exitFailure

