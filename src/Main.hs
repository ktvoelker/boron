
module Main where

import Data.Ini
import qualified Data.Text as T
import System.Environment

import Config
import Master
import Util

main :: IO ()
main = do
  getArgs >>= \case
    [configFilePath] -> do
      readIniFile configFilePath >>= \case
        Left msg -> abort $ T.pack msg
        Right ini -> parseConfig ini >>= runMaster
    _ -> abort "Usage: boron CONFIG"

