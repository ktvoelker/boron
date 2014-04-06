
module Master where

import qualified Data.Text as T
import Data.Text.Encoding
import Filesystem
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath, writeFile)
import System.Posix.Process
import Text.JSON

import Config
import Slave
import Util
import Web ()

masterFileName :: FilePath
masterFileName = fromText "builds.json"

writeMasterFile :: FilePath -> [T.Text] -> IO ()
writeMasterFile fp = writeFile fp . encodeUtf8 . T.pack . encodeStrict

runMaster :: Config -> IO ()
runMaster Config{..} = do
  ensureDirectory configOutputDir
  ensureDirectory configWorkDir
  writeMasterFile (configOutputDir </> masterFileName) (map buildName configBuilds)
  mapM_ (forkProcess . runSlave) configBuilds

