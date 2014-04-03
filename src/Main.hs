
module Main where

import Filesystem.Path.CurrentOS
import System.Environment

import Config
import Master
import Util

main :: IO ()
main = do
  getArgs >>= \case
    [configFilePath] -> parseConfig (decodeString configFilePath) >>= runMaster
    _ -> abort "Usage: boron CONFIG.json"

