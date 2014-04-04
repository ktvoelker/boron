
module Slave where

import Control.Category
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Functor
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time
import Filesystem
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath, writeFile, id, (.))
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

getFirstBuildNumber :: [FilePath] -> Integer
getFirstBuildNumber = (+ 1) . maximum . (0 :) . map f
  where
    f fp = if extensions fp == ["json"] && all isDigit bn then read bn else 0
      where
        bn = encodeString $ basename fp

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

minBuildNumberLength :: Int
minBuildNumberLength = 5

formatBuildNumber :: Integer -> T.Text
formatBuildNumber n = padding <> base
  where
    base = T.pack $ show n
    padding = T.replicate (max 0 (minBuildNumberLength - T.length base)) "0"

runBuild :: FilePath -> FilePath -> FilePath -> MVar Integer -> IO ()
runBuild builder outputDir summaryFile var = do
  buildNumber <- takeMVar var
  let numText = formatBuildNumber buildNumber
  let metaFileName = fromText numText <.> "json"
  let metaFile = outputDir </> metaFileName
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
  let summaryFileJSON = toJSObject [("latest", toText metaFileName)]
  let summaryFileBytes = encodeUtf8 . T.pack . encodeStrict $ summaryFileJSON
  writeFile summaryFile summaryFileBytes
  putMVar var $ buildNumber + 1

runSlave :: Build -> IO ()
runSlave Build{..} = do
  ensureDirectory buildOutputDir
  needSource <- not <$> isDirectory buildWorkDir
  when needSource $ getSource buildSource buildWorkDir
  buildVar <- listDirectory buildWorkDir >>= (getFirstBuildNumber >>> newMVar)
  setWorkingDirectory buildWorkDir
  let summaryFile = buildOutputDir </> "index.json"
  let doBuild = runBuild buildBuilder buildOutputDir summaryFile buildVar
  when needSource doBuild
  forever $ do
    wait buildPollInterval
    shouldBuild <- runPoll buildPoller
    when shouldBuild doBuild

