module Wolf3D.Utils (diffUTCTimeMillis) where

import Data.Time.Clock


diffUTCTimeMillis :: UTCTime -> UTCTime -> Int
diffUTCTimeMillis previous now = floor (diff * 1000)
  where
    diff = toRational (diffUTCTime now previous)
