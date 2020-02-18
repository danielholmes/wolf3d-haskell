module Main where

import Wolf3D.Display
import Wolf3D.Main
import Wolf3D.Loader
import Wolf3D.Debug.Dummy

main :: IO ()
main = createMain 1 dummyWorld $
  \r -> do
    setupRenderer r
    d <- loadRenderData r
    return (render r d)
