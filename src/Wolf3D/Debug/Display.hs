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
import Data.StateVar (($=), get)
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

  -- This is a bit of a hack - relies on knowing that the window size is scale * screenSize
  (SDL.V2 sX sY) <- get (SDL.rendererScale r)
  let miniMapWidth = round (((fromIntegral D.screenWidth) * sX) / 3)
  let miniMapHeight = round (((fromIntegral D.screenHeight) * sY) / 3)
  withScale r (SDL.V2 1.0 1.0) $
    withViewport r (Just (mkSDLRect 0 0 miniMapWidth miniMapHeight)) $
      renderMiniMap r 0.001 (miniMapWidth, miniMapHeight) w
  drawDebugText r drd debugText
  SDL.present r
  where
    w = simRunWorld sr
    runRender = do
      D.renderHud r rd
      D.renderWorld r rd w

createDebugText :: SimRun -> Integer -> String
createDebugText sr tookTime = unwords (map (\(l, v) -> l ++ ": " ++ v) items)
  where
    world = simRunWorld sr
    items = [("WT", show (worldTics world `div` 70) ++ "s"), ("Render", show tookTime ++ "ms")]

drawDebugText :: SDL.Renderer -> DebugRenderData -> String -> IO ()
drawDebugText r (DebugRenderData _ font) text =
  withViewport r (Just (mkSDLRect 0 0 D.screenWidth D.screenHeight)) $ do
    surface <- SDL.Font.solid font whiteColor (pack text)
    (SDL.V2 textW textH) <- SDL.surfaceDimensions surface
    texture <- SDL.createTextureFromSurface r surface
    let rect = Just (mkSDLRect 0 (D.screenHeight - textH) textW textH)
    SDL.rendererDrawColor r $= panelColor
    SDL.fillRect r rect
    SDL.copy r texture Nothing rect
    SDL.freeSurface surface
