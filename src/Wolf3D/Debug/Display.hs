{-# LANGUAGE OverloadedStrings #-}
module Wolf3D.Debug.Display (
  DebugRenderData (DebugRenderData),
  setupRenderer,
  render
) where

import qualified Wolf3D.Display as D
import Wolf3D.Engine
import Wolf3D.Runner
import Wolf3D.SDLUtils
import Wolf3D.Display.MiniMap
import Wolf3D.Display.Utils
import qualified SDL
import qualified SDL.Font
import Control.StopWatch
import Data.StateVar (($=))
import Data.Text (pack)
import System.Clock
import GHC.Word (Word8)


data DebugRenderData = DebugRenderData D.RenderData SDL.Font.Font

whiteColor :: SDL.V4 Word8
whiteColor = SDL.V4 255 255 255 255

panelColor :: SDL.V4 Word8
panelColor = SDL.V4 0 0 0 100

setupRenderer :: SDL.Renderer -> IO ()
setupRenderer = D.setupRenderer

render :: SDL.Renderer -> DebugRenderData -> SimRun -> IO ()
render r drd@(DebugRenderData rd _) sr = do
  (_, tookTime) <- stopWatch runRender
  let debugText = createDebugText sr (toNanoSecs tookTime `div` 1000000)
  withViewport r (Just (mkSDLRect 0 0 (fromIntegral miniMapWidth) (fromIntegral miniMapHeight))) $
    renderMiniMap r 0.009 (miniMapWidth, miniMapHeight) w
  drawDebugText r drd debugText
  SDL.present r
  where
    w = simRunWorld sr
    miniMapWidth = D.screenWidth `div` 3
    miniMapHeight = D.screenHeight `div` 3
    runRender = do
      D.renderHud r rd
      D.renderWorld r rd w

createDebugText :: SimRun -> Integer -> String
createDebugText sr tookTime = unwords (map (\(l, v) -> l ++ ": " ++ v) items)
  where
    world = simRunWorld sr
    items = [("WT", show (worldTime world `div` 1000) ++ "s"), ("Render", show tookTime ++ "ms")]

drawDebugText :: SDL.Renderer -> DebugRenderData -> String -> IO ()
drawDebugText r (DebugRenderData _ font) text =
  withViewport r (Just (mkSDLRect 0 0 (fromIntegral D.screenWidth) (fromIntegral D.screenHeight))) $ do
    surface <- SDL.Font.solid font whiteColor (pack text)
    (SDL.V2 textW textH) <- SDL.surfaceDimensions surface
    texture <- SDL.createTextureFromSurface r surface
    let rect = Just (mkSDLRect 0 (fromIntegral (D.screenHeight - fromIntegral textH)) textW textH)
    SDL.rendererDrawColor r $= panelColor
    SDL.fillRect r rect
    SDL.copy r texture Nothing rect
    SDL.freeSurface surface
