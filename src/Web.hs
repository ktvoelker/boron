
module Web
  ( WebConfig(..)
  , runWeb
  , forkWeb
  ) where

import Control.Concurrent
import Data.Monoid
import qualified Data.Text as T
import Filesystem.Path.CurrentOS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Prelude hiding (FilePath)

import Command

data WebConfig =
  WebConfig
  { webPort       :: Int
  , webStaticRoot :: FilePath
  , webUiRoot     :: FilePath
  , webBuildNames :: [T.Text]
  } deriving (Eq, Show)

fileApp :: FilePath -> Application
fileApp =
  staticApp
  . (\ss -> ss { ssLookupFile = ssLookupFile ss . drop 1})
  . defaultFileServerSettings

settings :: Int -> Settings
settings port = setPort port defaultSettings

buildCommands :: [(T.Text, BuildCommand)]
buildCommands = [("start", StartBuild), ("stop", StopBuild)]

emptyResponse :: Status -> Response
emptyResponse status = responseBuilder status [(hContentLength, "0")] mempty

notFound :: Response
notFound = emptyResponse notFound404

methodNotAllowed :: Response
methodNotAllowed = emptyResponse methodNotAllowed405

controlApp :: [T.Text] -> Chan Command -> Application
controlApp buildNames chan req = case pathInfo req of
  [_, "build", buildName, command]
    | Just buildCommand <- lookup command buildCommands
    , buildName `elem` buildNames -> do
      writeChan chan $ BuildCommand buildName buildCommand
      return $ emptyResponse accepted202
  _ -> return notFound

routerApp :: Application -> Application -> Application -> Application
routerApp ui output control req = case pathInfo req of
  [] ->
    if requestMethod req == methodGet
    then ui (req { pathInfo = ["ui", "index.html"] })
    else return methodNotAllowed
  ("ui" : _) ->
    if requestMethod req == methodGet
    then ui req
    else return methodNotAllowed
  ("output" : _) ->
    if requestMethod req == methodGet
    then output req
    else return methodNotAllowed
  ("control" : _) ->
    if requestMethod req == methodPost
    then control req
    else return methodNotAllowed
  _ -> return notFound

runWeb :: WebConfig -> Chan Command -> IO ()
runWeb WebConfig{..} chan =
  runSettings (settings webPort)
    $ routerApp
      (fileApp webUiRoot)
      (fileApp webStaticRoot)
      (controlApp webBuildNames chan)

forkWeb :: WebConfig -> IO (Chan Command, ThreadId)
forkWeb config = do
  chan <- newChan
  tid  <- forkIO $ runWeb config chan
  return (chan, tid)

