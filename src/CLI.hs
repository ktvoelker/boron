
module Main where

import Control.Monad
import Data.Functor
import Data.Monoid
import Network.HTTP
import Network.Stream
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: boron-control start BUILD"
  exitFailure

origin :: String
origin = "http://localhost:8015"

commandUrl :: [String] -> Maybe String
commandUrl ["start", name] = Just $ origin <> "/control/build/" <> name <> "/start"
commandUrl _ = Nothing

main :: IO ()
main = commandUrl <$> getArgs >>= maybe usage (run >=> output)

run :: String -> IO (Result Response_String)
run = simpleHTTP . postRequest

output :: Result Response_String -> IO ()
output (Left err) = print err
output (Right resp) = do
  print code
  if code >= 400
  then exitFailure
  else exitSuccess
  where
    (c100, c10, c1) = rspCode resp
    code = (c100 * 100) + (c10 * 10) + c1

