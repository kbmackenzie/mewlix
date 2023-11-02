module Mewlix.Utils.Time
( clockSec
) where

import Data.Int (Int64)
import Data.Functor ((<&>))
import System.Clock (Clock(..), TimeSpec(..), getTime)

nsecToSec :: Int64 -> Double
nsecToSec = (/ 1e9) . fromIntegral

timeAsSec :: TimeSpec -> Double
timeAsSec (TimeSpec secs nsecs) = fromIntegral secs + nsecToSec nsecs

clockSec :: IO Double
clockSec = getTime Realtime <&> timeAsSec
