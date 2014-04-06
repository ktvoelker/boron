
module Util where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Filesystem
import Filesystem.Path
import Prelude hiding (FilePath)
import System.Exit
import System.IO hiding (FilePath)

todo :: a
todo = error "Not implemented"

abort :: T.Text -> IO a
abort msg = do
  TIO.hPutStrLn stderr msg
  exitFailure

ensureDirectory :: FilePath -> IO ()
ensureDirectory = createDirectory True

traceValue :: (Show a) => a -> a
traceValue x = traceShow x x

