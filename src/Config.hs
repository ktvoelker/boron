
module Config where

import Control.Category
import Data.Functor
import qualified Data.HashMap.Strict as HMS
import Data.Ini
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import Filesystem.Path.CurrentOS
import Prelude hiding (id, (.), FilePath)

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
  { configMasterFile :: FilePath
  , configBuilds     :: [Build]
  } deriving (Show)

get :: T.Text -> T.Text -> HMS.HashMap T.Text a -> IO a
get section key hm = case HMS.lookup key hm of
  Nothing -> abort $ "Missing " <> section <> "." <> key
  Just value -> return value

masterFileName :: FilePath
masterFileName = fromText "builds.json"

parseConfig :: Ini -> IO Config
parseConfig ini = do
  output <- fromText <$> get "" "output" global
  work <- fromText <$> get "" "work" global
  let masterFile = output </> masterFileName
  Config masterFile <$> mapM (uncurry $ makeBuild output work) builds
  where
    hm = unIni ini
    global = maybe HMS.empty id $ HMS.lookup "" hm
    builds = filter (not . T.null . fst) $ HMS.toList hm

makeBuild :: FilePath -> FilePath -> T.Text -> HMS.HashMap T.Text T.Text -> IO Build
makeBuild output work name section = do
  source <- get' "source"
  intervalText <- get' "interval"
  poll <- fromText <$> get' "poll"
  build <- fromText <$> get' "build"
  interval <- case parseInterval intervalText of
    Nothing -> abort "Invalid interval"
    Just n -> return n
  return $
    Build
    { buildName = name
    , buildSource = source
    , buildPollInterval = interval
    , buildPoller = poll
    , buildBuilder = build
    , buildOutputDir = output </> fromText name
    , buildWorkDir = work </> fromText name
    }
  where
    get' = flip (get name) section

parseInterval :: T.Text -> Maybe NominalDiffTime
parseInterval = T.unpack >>> reads >>> \case
  [(ndt, "")] -> Just $ fromInteger ndt
  _ -> Nothing

