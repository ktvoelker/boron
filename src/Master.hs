
module Master where

import Control.Concurrent
import Control.Monad
import Data.Functor
import qualified Data.Text as T
import Data.Text.Encoding
import Filesystem
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath, writeFile)
import Text.JSON

import Command
import Config
import Paths_boron
import Slave
import Util
import Web

webConfig :: Config -> IO WebConfig
webConfig Config{..} = do
  webUi <- maybe (decodeString <$> getDataFileName "ui") return configWebUi
  return $ WebConfig
    { webPort       = configWebPort
    , webStaticRoot = configOutputDir
    , webUiRoot     = webUi
    , webBuildNames = map buildName configBuilds
    }

masterFileName :: FilePath
masterFileName = fromText "builds.json"

writeMasterFile :: FilePath -> [T.Text] -> IO ()
writeMasterFile fp = writeFile fp . encodeUtf8 . T.pack . encodeStrict

handleCommand :: [(T.Text, Chan BuildCommand)] -> Command -> IO ()
handleCommand builds = \case
  BuildCommand name buildCommand -> case lookup name builds of
    Nothing -> todo
    Just chan -> writeChan chan buildCommand

runMaster :: Config -> IO ()
runMaster config@Config{..} = do
  ensureDirectory configOutputDir
  ensureDirectory configWorkDir
  writeMasterFile (configOutputDir </> masterFileName) (map buildName configBuilds)
  buildChans <- mapM (const newChan) configBuilds
  mapM_ (forkIO . uncurry runSlave) $ zip configBuilds buildChans
  (controlChan, _) <- webConfig config >>= forkWeb
  let
  { namedBuildChans
    = zipWith (\build chan -> (buildName build, chan)) configBuilds buildChans
  }
  forever $ readChan controlChan >>= handleCommand namedBuildChans

