
module Slave where

import Control.Concurrent
import Control.Monad
import Data.Functor
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time
import Filesystem
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath, writeFile)
import System.Exit
import System.Process
import Text.JSON

import Config
import Util

run :: String -> [String] -> IO ExitCode
run cmd args = f <$> readProcessWithExitCode cmd args ""
  where
    f (result, _, _) = result

displayFilePath :: FilePath -> T.Text
displayFilePath = either id id . toText

getSource :: T.Text -> FilePath -> IO ()
getSource source workDir =
  run "git" ["clone", T.unpack source, encodeString workDir] >>= \case
    ExitSuccess -> return ()
    ExitFailure _ ->
      abort $ "Failed to clone " <> source <> " to " <> displayFilePath workDir

microsecondsPerSecond :: (Num a) => a
microsecondsPerSecond = 1000000

wait :: NominalDiffTime -> IO ()
wait = threadDelay . round . (* microsecondsPerSecond) . toRational

runPoll :: FilePath -> IO Bool
runPoll fp = (/= ExitSuccess) <$> run (encodeString fp) []

getFirstBuildNumber :: IO Integer
getFirstBuildNumber = todo

timeToJSValue :: UTCTime -> JSValue
timeToJSValue = showJSON . show

makeMetaFile :: UTCTime -> UTCTime -> ExitCode -> JSObject JSValue
makeMetaFile startTime endTime result =
  toJSObject
    [ ("start", timeToJSValue startTime)
    , ("end", timeToJSValue endTime)
    , ("ok", showJSON $ result == ExitSuccess)
    , ("code", code)
    ]
  where
    code = case result of
      ExitSuccess -> JSNull
      ExitFailure code -> showJSON code

runBuild :: FilePath -> FilePath -> MVar Integer -> IO ()
runBuild builder outputDir var = do
  buildNumber <- takeMVar var
  let numText = T.pack $ show buildNumber
  let metaFile = outputDir </> (fromText numText <.> "json")
  let stdOutFile = outputDir </> (fromText (numText <> "-out") <.> "txt")
  let stdErrFile = outputDir </> (fromText (numText <> "-err") <.> "txt")
  startTime <- getCurrentTime
  -- Work around for https://ghc.haskell.org/trac/ghc/ticket/8448
  let emptyList = [] :: [String]
  result <-
    withTextFile stdOutFile WriteMode $ \outHandle ->
      withTextFile stdErrFile WriteMode $ \errHandle -> do
        (_, _, _, p) <- createProcess (proc (encodeString builder) emptyList)
          { std_out = UseHandle outHandle
          , std_err = UseHandle errHandle
          }
        waitForProcess p
  endTime <- getCurrentTime
  let metaFileJSON = makeMetaFile startTime endTime result
  let metaFileBytes = encodeUtf8 . T.pack . encodeStrict $ metaFileJSON
  writeFile metaFile metaFileBytes
  putMVar var $ buildNumber + 1

runSlave :: Build -> IO ()
runSlave Build{..} = do
  needSource <- not <$> isDirectory buildWorkDir
  when needSource $ getSource buildSource buildWorkDir
  setWorkingDirectory buildWorkDir
  buildVar <- getFirstBuildNumber >>= newMVar
  let doBuild = runBuild buildBuilder buildOutputDir buildVar
  when needSource doBuild
  forever $ do
    wait buildPollInterval
    shouldBuild <- runPoll buildPoller
    when shouldBuild doBuild

