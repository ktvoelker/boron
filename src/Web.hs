
module Web
  ( runWeb
  ) where

import Control.Concurrent.Chan
import Data.Monoid
import qualified Data.Text as T
import Filesystem.Path.CurrentOS
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Application.Static as Stat
import qualified Network.Wai.Handler.Warp as Warp
import Prelude hiding (FilePath)

import Command

data WebConfig =
  WebConfig
  { webPort       :: Int
  , webStaticRoot :: FilePath
  , webBuildNames :: [T.Text]
  } deriving (Eq, Show)

staticApp :: FilePath -> Application
staticApp = Stat.staticApp . Stat.defaultFileServerSettings

settings :: Int -> Warp.Settings
settings port = Warp.defaultSettings
  { Warp.settingsPort = port
  }

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

routerApp :: Application -> Application -> Application
routerApp static control req = case pathInfo req of
  ("output" : _)  ->
    if requestMethod req == methodGet
    then static req
    else return methodNotAllowed
  ("control" : _) ->
    if requestMethod req == methodPost
    then control req
    else return methodNotAllowed
  _ -> return notFound

runWeb :: WebConfig -> IO (Chan Command)
runWeb WebConfig{..} = do
  chan <- newChan
  Warp.runSettings (settings webPort)
    $ routerApp (staticApp webStaticRoot) (controlApp webBuildNames chan)
  return chan

