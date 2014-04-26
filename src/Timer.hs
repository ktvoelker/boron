
module Timer where

import Control.Concurrent
import Data.Functor
import Data.Time

microsecondsPerSecond :: (Num a) => a
microsecondsPerSecond = 1000000

wait :: NominalDiffTime -> IO ()
wait = threadDelay . round . (* microsecondsPerSecond) . toRational

startTimer :: NominalDiffTime -> IO () -> IO ()
startTimer dt action = void . forkIO $ wait dt >> action

