
module Config where

import qualified Data.HashMap.Strict as HMS
import Data.Ini
import Data.Monoid
import qualified Data.Text as T
import Data.Time

import Util

data Build =
  Build
  { buildName         :: T.Text
  , buildInitializer  :: T.Text
  , buildPollInterval :: NominalDiffTime
  , buildPoller       :: T.Text
  , buildBuilder      :: T.Text
  , buildOutputDir    :: T.Text
  , buildWorkDir      :: T.Text
  } deriving (Eq, Ord, Show)

get :: T.Text -> T.Text -> HMS.HashMap T.Text a -> IO a
get section key hm = case HMS.lookup key hm of
  Nothing -> abort $ "Missing " <> section <> "." <> key
  Just value -> return value

parseConfig :: Ini -> IO [Build]
parseConfig ini = do
  output <- get "" "output" global
  work <- get "" "work" global
  mapM (uncurry $ makeBuild output work) builds
  where
    hm = unIni ini
    global = maybe HMS.empty id $ HMS.lookup "" hm
    builds = filter (not . T.null . fst) $ HMS.toList hm

makeBuild :: T.Text -> T.Text -> T.Text -> HMS.HashMap T.Text T.Text -> IO Build
makeBuild output work name section = do
  init <- get' "init"
  intervalText <- get' "interval"
  poll <- get' "poll"
  build <- get' "build"
  interval <- case parseInterval intervalText of
    Nothing -> abort "Invalid interval"
    Just n -> return n
  return $
    Build
    { buildName = name
    , buildInitializer = init
    , buildPollInterval = interval
    , buildPoller = poll
    , buildBuilder = build
    , buildOutputDir = output <> "/" <> name
    , buildWorkDir = work <> "/" <> name
    }
  where
    get' = flip (get name) section

parseInterval :: T.Text -> Maybe NominalDiffTime
parseInterval = todo

