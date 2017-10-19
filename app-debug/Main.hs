module Main where

import Wolf3D.Display
import Wolf3D.UI
import Wolf3D.Runner
import Wolf3D.Types
import Wolf3D.Debug.Data

main :: IO ()
main = createUI $
  \r -> runLoop dummyWorld (posInt 16) (render r)
