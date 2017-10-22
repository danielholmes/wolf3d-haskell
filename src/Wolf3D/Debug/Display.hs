{-# LANGUAGE OverloadedStrings #-}
module Wolf3D.Debug.Display (
  DebugRenderData (DebugRenderData),
  setupRenderer,
  render
) where

import qualified Wolf3D.Display as D
import Wolf3D.World
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


data DebugRenderData = DebugRenderData D.RenderData SDL.Font.Font

setupRenderer :: SDL.Renderer -> IO ()
setupRenderer = D.setupRenderer

render :: SDL.Renderer -> DebugRenderData -> SimRun -> IO ()
render r drd@(DebugRenderData rd@(D.RenderData (width, height) _ _ _ _) _) sr = do
  (_, tookTime) <- stopWatch runRender
  let debugText = createDebugText sr (toNanoSecs tookTime `div` 1000000)
  withViewport r (Just (mkSDLRect 0 0 (fromIntegral miniMapWidth) (fromIntegral miniMapHeight))) $
    renderMiniMap r 0.009 (miniMapWidth, miniMapHeight) w
  drawDebugText r drd debugText
  SDL.present r
  where
    w = simRunWorld sr
    miniMapWidth = width `div` 3
    miniMapHeight = height `div` 3
    runRender = D.renderWorld r rd w

createDebugText :: SimRun -> Integer -> String
createDebugText sr tookTime = unwords (map (\(l, v) -> l ++ ": " ++ v) items)
  where
    world = simRunWorld sr
    items = [("WT", show (worldTime world `div` 1000) ++ "s"), ("Render", show tookTime ++ "ms")]

drawDebugText :: SDL.Renderer -> DebugRenderData -> String -> IO ()
drawDebugText r (DebugRenderData (D.RenderData (w, h) _ _ _ _) font) text =
  withViewport r (Just (mkSDLRect 0 0 (fromIntegral w) (fromIntegral h))) $ do
    surface <- SDL.Font.solid font (SDL.V4 255 255 255 255) (pack text)
    (SDL.V2 textW textH) <- SDL.surfaceDimensions surface
    texture <- SDL.createTextureFromSurface r surface
    let rect = Just (mkSDLRect 0 (fromIntegral (h - fromIntegral textH)) textW textH)
    SDL.rendererDrawColor r $= SDL.V4 0 0 0 122
    SDL.fillRect r rect
    SDL.copy r texture Nothing rect
    SDL.freeSurface surface
