module Main where

import Wolf3D.Main
import Wolf3D.Debug.Data
import Wolf3D.Debug.Dummy
import Wolf3D.Debug.Display

main :: IO ()
main = createMain 3 dummyWorld $
  \r -> do
    setupRenderer r
    d <- loadRenderData r
    return (render r d)
