
module Config where

import Control.Applicative
import Control.Category
import Control.Monad
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time
import Filesystem
import Filesystem.Path.CurrentOS
import Prelude hiding (id, (.), FilePath, readFile)
import Text.JSON

import Util

data Build =
  Build
  { buildName         :: T.Text
  , buildSource       :: T.Text
  , buildPollInterval :: NominalDiffTime
  , buildPoller       :: FilePath
  , buildBuilder      :: FilePath
  , buildOutputDir    :: FilePath
  , buildWorkDir      :: FilePath
  } deriving (Show)

data Config =
  Config
  { configOutputDir  :: FilePath
  , configWorkDir    :: FilePath
  , configWebPort    :: Int
  , configWebUi      :: Maybe FilePath
  , configBuilds     :: [Build]
  } deriving (Show)

type JSReader a = JSValue -> Result a

instance JSON FilePath where
  showJSON = showJSON . toText
  readJSON = fmap fromText . readJSON
  
instance JSON NominalDiffTime where
  showJSON = showJSON . (fromRational :: Rational -> Double) . toRational
  readJSON = fmap (fromRational . (toRational :: Double -> Rational)) . readJSON

readBuild :: T.Text -> FilePath -> FilePath -> JSReader Build
readBuild name output work (JSObject obj) =
  Build name
  <$> valFromObj "source" obj
  <*> valFromObj "interval" obj
  <*> valFromObj "poll" obj
  <*> valFromObj "build" obj
  <*> pure output
  <*> pure work
readBuild _ _ _ _ = Error "A build must be a JSON object"

readBuilds :: FilePath -> FilePath -> JSReader [Build]
readBuilds output work = decJSDict "Builds" >=> mapM (uncurry f)
  where
    f key = readBuild key (output </> key') (work </> key')
      where
        key' = fromText key

instance JSKey T.Text where
  toJSKey = T.unpack
  fromJSKey = Just . T.pack

absolutize :: FilePath -> FilePath -> FilePath
absolutize root rel = collapse $ root </> rel

readConfig :: FilePath -> JSReader Config
readConfig root (JSObject obj) = do
  output  <- absolutize root <$> valFromObj "output" obj
  work    <- absolutize root <$> valFromObj "work" obj
  webPort <- valFromObj "web-port" obj
  webUi   <- (Just <$> valFromObj "web-ui" obj) <|> pure Nothing
  builds  <- valFromObj "builds" obj >>= readBuilds output work
  return $ Config output work webPort webUi builds
readConfig _ _ = Error "A config must be a JSON object"

parseConfig :: FilePath -> IO Config
parseConfig fp = do
  fp' <- absolutize <$> getWorkingDirectory <*> pure fp
  (f (directory fp') <$> readFile fp') >>= abortOnError
  where
    abortOnError (Ok x) = return x
    abortOnError (Error msg) = abort $ T.pack msg
    f root = decodeUtf8 >>> T.unpack >>> decodeStrict >=> readConfig root

