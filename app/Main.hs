module Main where

import Wolf3D.Display
import Wolf3D.UI
import Wolf3D.Runner
import Wolf3D.Types
import Wolf3D.Data
import Wolf3D.Debug.Dummy

main :: IO ()
main = createUI $
  \r s -> do
    setupRenderer r
    d <- loadRenderData r s
    runLoop dummyWorld (posInt 16) (render r d)
