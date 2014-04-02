
module Master where

import qualified Data.Text as T
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)
import System.Posix.Process
import Text.JSON

import Config
import Slave

writeMasterFile :: FilePath -> [T.Text] -> IO ()
writeMasterFile fp = writeFile (encodeString fp) . encodeStrict

runMaster :: Config -> IO ()
runMaster Config{..} = do
  writeMasterFile configMasterFile (map buildName configBuilds)
  mapM_ (forkProcess . runSlave) configBuilds

