
module Slave where

import Control.Category
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Functor
import Data.Maybe
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

run :: Maybe FilePath -> String -> [String] -> IO ExitCode
run wd cmd args = do
  (_, _, _, ph) <- createProcess $ (proc cmd args) { cwd = encodeString <$> wd }
  waitForProcess ph

displayFilePath :: FilePath -> T.Text
displayFilePath = either id id . toText

getSource :: T.Text -> FilePath -> IO ()
getSource source workDir =
  run Nothing "git" ["clone", T.unpack source, encodeString workDir] >>= \case
    ExitSuccess -> return ()
    ExitFailure _ ->
      abort $ "Failed to clone " <> source <> " to " <> displayFilePath workDir

microsecondsPerSecond :: (Num a) => a
microsecondsPerSecond = 1000000

wait :: NominalDiffTime -> IO ()
wait = threadDelay . round . (* microsecondsPerSecond) . toRational

runPoll :: FilePath -> FilePath -> IO Bool
runPoll wd fp = (/= ExitSuccess) <$> run (Just wd) (encodeString fp) []

isMetaFile :: FilePath -> Bool
isMetaFile = isJust . buildNumber

metaFiles :: [FilePath] -> [FilePath]
metaFiles = filter isMetaFile

buildNumbers :: [FilePath] -> [Integer]
buildNumbers = catMaybes . map buildNumber

buildNumber :: FilePath -> Maybe Integer
buildNumber fp =
  if extensions fp == ["json"] && all isDigit bn
  then Just $ read bn
  else Nothing
  where
    bn = encodeString $ basename fp

firstBuildNumber :: [FilePath] -> Integer
firstBuildNumber = (+ 1) . maximum . (0 :) . buildNumbers

getMetaFiles :: FilePath -> IO [FilePath]
getMetaFiles = fmap metaFiles . listDirectory

getBuildNumbers :: FilePath -> IO [Integer]
getBuildNumbers = fmap buildNumbers . listDirectory

getFirstBuildNumber :: FilePath -> IO Integer
getFirstBuildNumber = fmap firstBuildNumber . listDirectory

timeToJSValue :: UTCTime -> JSValue
timeToJSValue = showJSON . show

makeRunningMetaFile :: Integer -> UTCTime -> JSObject JSValue
makeRunningMetaFile num startTime =
  toJSObject
    [ ("number", showJSON num)
    , ("start", timeToJSValue startTime)
    , ("status", showJSON ("running" :: T.Text))
    ]

makeMetaFile :: Integer -> UTCTime -> UTCTime -> ExitCode -> JSObject JSValue
makeMetaFile num startTime endTime result =
  toJSObject
  $ [ ("number", showJSON num)
    , ("start", timeToJSValue startTime)
    , ("end", timeToJSValue endTime)
    , ("status", showJSON $ if result == ExitSuccess then "pass" :: T.Text else "fail")
    ]
  ++ code
  where
    code = case result of
      ExitSuccess -> []
      ExitFailure code -> [("code", showJSON code)]

minBuildNumberLength :: Int
minBuildNumberLength = 5

formatBuildNumber :: Integer -> T.Text
formatBuildNumber = T.pack . show

makeSummaryFile :: [FilePath] -> JSValue
makeSummaryFile = showJSONs . map (encodeString . basename)

runBuild :: FilePath -> FilePath -> FilePath -> FilePath -> MVar Integer -> IO ()
runBuild wd builder outputDir summaryFile var = do
  buildNumber <- takeMVar var
  let numText = formatBuildNumber buildNumber
  let metaFileName = fromText numText <.> "json"
  let metaFile = outputDir </> metaFileName
  let stdOutFile = outputDir </> (fromText (numText <> "-out") <.> "txt")
  let stdErrFile = outputDir </> (fromText (numText <> "-err") <.> "txt")
  startTime <- getCurrentTime
  let metaFileJSON = makeRunningMetaFile buildNumber startTime
  let metaFileBytes = encodeUtf8 . T.pack . encodeStrict $ metaFileJSON
  writeFile metaFile metaFileBytes
  summaryFileJSON <- makeSummaryFile <$> getMetaFiles outputDir
  let summaryFileBytes = encodeUtf8 . T.pack . encodeStrict $ summaryFileJSON
  writeFile summaryFile summaryFileBytes
  -- Work around for https://ghc.haskell.org/trac/ghc/ticket/8448
  let emptyList = [] :: [String]
  result <-
    withTextFile stdOutFile WriteMode $ \outHandle ->
      withTextFile stdErrFile WriteMode $ \errHandle -> do
        (_, _, _, p) <- createProcess (proc (encodeString builder) emptyList)
          { std_out = UseHandle outHandle
          , std_err = UseHandle errHandle
          , cwd     = Just $ encodeString wd
          }
        waitForProcess p
  endTime <- getCurrentTime
  let metaFileJSON = makeMetaFile buildNumber startTime endTime result
  let metaFileBytes = encodeUtf8 . T.pack . encodeStrict $ metaFileJSON
  writeFile metaFile metaFileBytes
  putMVar var $ buildNumber + 1

runSlave :: Build -> IO ()
runSlave Build{..} = do
  ensureDirectory buildOutputDir
  buildVar <- getFirstBuildNumber buildOutputDir >>= newMVar
  needSource <- not <$> isDirectory buildWorkDir
  when needSource $ getSource buildSource buildWorkDir
  let summaryFile = buildOutputDir </> "index.json"
  let doBuild = runBuild buildWorkDir buildBuilder buildOutputDir summaryFile buildVar
  forever $ do
    wait buildPollInterval
    shouldBuild <- runPoll buildWorkDir buildPoller
    when shouldBuild doBuild

