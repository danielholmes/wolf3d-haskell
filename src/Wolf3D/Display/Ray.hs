module Wolf3D.Display.Ray (
  castRayToWalls,
  castRaysToWalls,
  HitDirection (..),
  focalLength,
  hitDistance
) where

import Data.Vector
import Wolf3D.WorldData
import Wolf3D.Geom
import Data.Bits
import Data.Array
import Foreign.C.Types (CInt)
import Wolf3D.Display.Data

-- This is the distance to projection plane
-- Rendering is done by moving the camera backwards from the player position by this length,
-- then using the player's position as the projection plane
-- NOTE: Check this, seems that focal length is probably meant to go infront of player
focalLength :: Int
focalLength = 0x5700

allFineAngles :: [FineAngle]
allFineAngles = [0..(fineAngles - 1)]

sinTable :: Array Angle Double
sinTable = array (0, fineAngles - 1) [(i, sin (fineToNormalAngle i * degToRad)) | i <- allFineAngles]

focalSinTable :: Array Angle Double
focalSinTable = listArray (0, fineAngles - 1) (map (* fromIntegral focalLength) (elems sinTable))

cosTable :: Array Angle Double
cosTable = array (0, fineAngles - 1) [(i, cos (fineToNormalAngle i * degToRad)) | i <- allFineAngles]

focalCosTable :: Array Angle Double
focalCosTable = listArray (0, fineAngles - 1) (map (* fromIntegral focalLength) (elems cosTable))

tanTable :: Array FineAngle Double
tanTable = array (0, fineAngles - 1) [(i, tan (fineToNormalAngle i * degToRad)) | i <- allFineAngles]

data RayData = DiagonalRayData {focal :: (Int, Int)
                                , fineViewAngleOffset :: FineAngle
                                , horInterceptYTile :: Int
                                , verInterceptXTile :: Int
                                , xTileStep :: Int
                                , yTileStep :: Int
                                , xStep :: Int
                                , yStep :: Int
                                , horNextIntercept :: (Int, Int)
                                , verNextIntercept :: (Int, Int)} |
                HorizontalRayData {focal :: (Int, Int)
                                  , fineViewAngleOffset :: FineAngle
                                  , interceptY :: Int
                                  , interceptXTile :: Int
                                  , tileStep :: Int} |
                VerticalRayData {focal :: (Int, Int)
                                , fineViewAngleOffset :: FineAngle
                                , interceptX :: Int
                                , interceptYTile :: Int
                                , tileStep :: Int}
  deriving (Show)

fineViewAngleOffsets :: [FineAngle]
fineViewAngleOffsets = reverse (map pixelToFineAngle [0..(actionWidth - 1)])
  where
    fineFieldOfView = normalToFineAngle fieldOfView

    pixelToFineAngle :: CInt -> FineAngle
    pixelToFineAngle p = round (fromIntegral fineFieldOfView * pixelRatio)
      where pixelRatio = ((fromIntegral p / fromIntegral (actionWidth - 1)) - 0.5) :: Double

fine270 :: Int
fine270 = normalToFineAngle 270

fine90 :: Int
fine90 = normalToFineAngle 90

fine180 :: Int
fine180 = normalToFineAngle 180

fine360 :: Int
fine360 = normalToFineAngle 360

castRaysToWalls :: WallMap -> Vector2 -> FineAngle -> [WallRayHit]
castRaysToWalls wm pos fMA = map (\o -> castRayToWalls wm pos fMA o) fineViewAngleOffsets

castRayToWalls :: WallMap -> Vector2 -> FineAngle -> FineAngle -> WallRayHit
castRayToWalls wm pos fMA fAO = nextRayCheck wm (focal rd) rd
  where rd = createRayData pos fMA fAO

createRayData :: Vector2 -> FineAngle -> FineAngle -> RayData
createRayData pos@(Vector2 x y) fMA fineVAO
  | fineViewAngle == 0 || 
    fineViewAngle == fine180  = HorizontalRayData {focal=focal1
                                                  , fineViewAngleOffset=fineVAO
                                                  , interceptY=focalYI
                                                  , interceptXTile=verInterceptXTile1
                                                  , tileStep=xTileStep1}
  | fineViewAngle == fine90 ||
    fineViewAngle == fine270  = VerticalRayData {focal=focal1
                                                , fineViewAngleOffset=fineVAO
                                                , interceptX=focalXI
                                                , interceptYTile=horInterceptYTile1
                                                , tileStep=yTileStep1}
  | is0To89     = let
                    tanTheta = tanTable!fineViewAngle
                    -- aX = focalXI + round (fromIntegral (focalYI - focalTileY * tileGlobalSize) * tanTheta)
                    aXY = fromIntegral (focalYI - (focalTileY * tileGlobalSize))
                    aX = focalXI + round (aXY / tanTheta)
                   
                    -- TODO: Use focalXI shift instead, or partial?
                    bYX = fromIntegral (((focalTileX + 1) * tileGlobalSize) - focalXI)
                    bY = focalYI - round (bYX * tanTheta)
                  in
                    DiagonalRayData {focal=focal1
                                    , fineViewAngleOffset=fineVAO
                                    , horNextIntercept=(aX, (horInterceptYTile1 + 1) * tileGlobalSize)
                                    , verNextIntercept=((focalTileX + xTileStep1) * tileGlobalSize, bY)
                                    , horInterceptYTile=horInterceptYTile1
                                    , verInterceptXTile=focalTileX + xTileStep1
                                    , xTileStep=xTileStep1
                                    , yTileStep=yTileStep1
                                    , xStep=(round (fromIntegral tileGlobalSize * tanTable!(fine90 - fineViewAngle)))
                                    , yStep=(-round (fromIntegral tileGlobalSize * tanTheta))}
  | is90To179   = let
                    tanTheta = tanTable!(fineViewAngle - fine90)
                    aY = focalTileY * tileGlobalSize
                    aX = focalXI - round (fromIntegral (focalYI - aY) * tanTheta)
                    -- TODO: Use focalXI shift instead
                    bX = fromIntegral (focalXI - focalTileX * tileGlobalSize)
                    bY = round (focalY - bX / tanTheta)
                  in
                    DiagonalRayData {focal=focal1
                                    , fineViewAngleOffset=fineVAO
                                    , horNextIntercept=(aX, (horInterceptYTile1 + 1) * tileGlobalSize)
                                    , verNextIntercept=(focalTileX * tileGlobalSize, bY)
                                    , horInterceptYTile=horInterceptYTile1
                                    , verInterceptXTile=focalTileX + xTileStep1
                                    , xTileStep=xTileStep1
                                    , yTileStep=yTileStep1
                                    , xStep=(-round (fromIntegral tileGlobalSize * tanTheta))
                                    , yStep=(-round (fromIntegral tileGlobalSize / tanTheta))}
  | is180To269                = let
                                  tanTheta = tanTable!(fine270 - fineViewAngle)
                                  aX = focalXI - round (fromIntegral (((focalTileY + yTileStep1) * tileGlobalSize) - focalYI) * tanTheta)
                                  -- TODO: Use focalXI shift instead
                                  bYX = fromIntegral (focalXI - (focalTileX * tileGlobalSize))
                                  bY = focalYI + round (bYX / tanTheta)
                                in
                                  DiagonalRayData {focal=focal1
                                                  , fineViewAngleOffset=fineVAO
                                                  , horNextIntercept=(aX, horInterceptYTile1 * tileGlobalSize)
                                                  , verNextIntercept=(focalTileX * tileGlobalSize, bY)
                                                  , horInterceptYTile=horInterceptYTile1
                                                  , verInterceptXTile=focalTileX + xTileStep1
                                                  , xTileStep=xTileStep1
                                                  , yTileStep=yTileStep1
                                                  , xStep=(- round (fromIntegral tileGlobalSize * tanTheta))
                                                  , yStep=(round (fromIntegral tileGlobalSize * tanTable!(fineViewAngle - fine180)))}
  | is270To359                = let
                                  tanTheta = tanTable!(fineViewAngle - fine270)
                                  aX = focalXI + round (fromIntegral (((focalTileY + yTileStep1) * tileGlobalSize) - focalYI) * tanTheta)
                                  bY = focalYI + round (fromIntegral (((focalTileX + xTileStep1) * tileGlobalSize) - focalXI) / tanTheta)
                                in
                                  DiagonalRayData {focal=focal1
                                                  , fineViewAngleOffset=fineVAO
                                                  , horNextIntercept=(aX, horInterceptYTile1 * tileGlobalSize)
                                                  , verNextIntercept=((focalTileX + xTileStep1) * tileGlobalSize, bY)
                                                  , horInterceptYTile=horInterceptYTile1
                                                  , verInterceptXTile=focalTileX + xTileStep1
                                                  , xTileStep=xTileStep1
                                                  , yTileStep=yTileStep1
                                                  , xStep=(round (fromIntegral tileGlobalSize * tanTheta))
                                                  , yStep=(round (fromIntegral tileGlobalSize * tanTable!(fine360 - fineViewAngle)))}
  | fineViewAngle < 0         = createRayData pos (fMA + fine360) fineVAO
  | fineViewAngle >= fine360  = createRayData pos (fMA - fine360) fineVAO
  | otherwise                 = error "Shouldn't be able to match this"
  where
    fineViewAngle = fMA + fineVAO
    
    viewSin = focalSinTable!fineViewAngle
    viewCos = focalCosTable!fineViewAngle
    focalX = x - viewCos
    focalY = y + viewSin
    focalXI :: Int
    focalXI = floor focalX
    focalYI :: Int
    focalYI = floor focalY
    focal1 = (focalXI, focalYI)
    focalTileX = focalXI `shiftR` tileToGlobalShift
    focalTileY = focalYI `shiftR` tileToGlobalShift
--    xPartialDown = focalXI .&. (tileGlobalSize - 1)
--    xPartialUp = tileGlobalSize - xPartialDown
--    yPartialDown = focalYI .&. (tileGlobalSize - 1)
--    yPartialUp = tileGlobalSize - yPartialDown

    -- Taken directly from source, find a better way
    is0To89 = fineViewAngle >= 0 && fineViewAngle < fine90
    is90To179 = fineViewAngle >= fine90 && fineViewAngle < fine180
    is180To269 = fineViewAngle >= fine180 && fineViewAngle < fine270
    is270To359 = fineViewAngle >= fine270 && fineViewAngle < fine360

    xTileStep1 = if is0To89 || is270To359 then 1 else -1 :: Int
    yTileStep1 = if is180To269 || is270To359 then 1 else -1 :: Int

--    xPartial = if is0To89 || is270To359 then xPartialUp else xPartialDown
--    yPartial = if is180To269 || is270To359 then yPartialUp else yPartialDown

    horInterceptYTile1 = focalTileY + yTileStep1
    verInterceptXTile1 = focalTileX + xTileStep1

nextRayCheck :: WallMap -> (Int, Int) -> RayData -> WallRayHit
nextRayCheck wm (_, y) nextD@DiagonalRayData{ horNextIntercept=(_, hY)
                                            , verNextIntercept=(_, vY)}
  | nextHorYStep < nextVerYStep = horRayCheck wm nextD
  | otherwise                   = verRayCheck wm nextD
  where
    nextHorYStep = abs (hY - y)
    nextVerYStep = abs (vY - y)
nextRayCheck wm _ d@HorizontalRayData {} = verRayCheck wm d
nextRayCheck wm _ d@VerticalRayData {} = horRayCheck wm d

verRayCheck :: WallMap -> RayData -> WallRayHit
verRayCheck _ (VerticalRayData {}) = error "Shouldn't be called"
verRayCheck wm d@DiagonalRayData {focal=f
                                  , fineViewAngleOffset=vAO
                                  , verInterceptXTile=vIXTile
                                  , xTileStep=xTileS
                                  , yStep=dy
                                  , verNextIntercept=currentIntercept@(x, y)} = case hitting of
    Nothing -> nextRayCheck wm currentIntercept nextD
    Just m  -> WallRayHit {material=m
                           , direction=Vertical
                          , distance=hitDistance vAO f currentIntercept
                          , tilePosition=y `mod` tileGlobalSize}
  where
    vIYTile = (y - 1) `shiftR` tileToGlobalShift
    -- intXTile = if xTileS == -1 then vIXTile + 1 else vIXTile
    hitting = wm!(vIXTile, vIYTile)
    -- vIntercept = (intXTile * tileGlobalSize, vIY)

    vInterceptNext = (x + xTileS * tileGlobalSize, y + dy)
    nextD = d{verInterceptXTile=(vIXTile + xTileS)
              , verNextIntercept=vInterceptNext}

verRayCheck wm d@HorizontalRayData {focal=f
                                   , fineViewAngleOffset=vAO
                                   , interceptY=vIY
                                   , interceptXTile=vIXTile
                                   , tileStep=xTileS} = case hitting of
    Nothing -> verRayCheck wm d{interceptXTile=(vIXTile + xTileS)}
    Just m  -> WallRayHit {material=m
                          , direction=Vertical
                          , distance=hitDistance vAO f vIntercept
                          , tilePosition=vIY `mod` tileGlobalSize}
  where
    vIYTile = (vIY - 1) `shiftR` tileToGlobalShift
    hitting = wm!(vIXTile, vIYTile)

    vInterceptX = vIXTile * tileGlobalSize
    vIntercept = (if xTileS == -1 then vInterceptX + tileGlobalSize else vInterceptX, vIY)

horRayCheck :: WallMap -> RayData -> WallRayHit
horRayCheck _ (HorizontalRayData {}) = error "Shouldn't be called"
horRayCheck wm d@DiagonalRayData {focal=f
                                 , fineViewAngleOffset=vAO
                                 , horInterceptYTile=hIYTile
                                 , yTileStep=dYTile
                                 , xStep=dX
                                 , horNextIntercept=currentIntercept@(x, y)} = case hitting of
    Nothing -> nextRayCheck wm currentIntercept nextD
    Just m  -> WallRayHit {material=m
                          , direction=Horizontal
                          , distance=hitDistance vAO f currentIntercept
                          , tilePosition=x `mod` tileGlobalSize}
  where
    hIXTile = (x - 1) `shiftR` tileToGlobalShift
    hitting = wm!(hIXTile, hIYTile)

    hInterceptNext = (x + dX, y + dYTile * tileGlobalSize)
    nextD = d{horInterceptYTile=(hIYTile + dYTile)
            , horNextIntercept=hInterceptNext}
horRayCheck wm d@VerticalRayData {focal=f
                                 , fineViewAngleOffset=vAO
                                 , interceptX=hIX
                                 , interceptYTile=hIYTile
                                 , tileStep=dYTile} = case hitting of
    Nothing -> horRayCheck wm d{interceptYTile=(hIYTile + dYTile)}
    Just m  -> WallRayHit {material=m
                           , direction=Horizontal
                           , distance=hitDistance vAO f hIntercept
                           , tilePosition=hIX `mod` tileGlobalSize}
  where
    hIXTile = (hIX - 1) `shiftR` tileToGlobalShift
    hitting = wm!(hIXTile, hIYTile)

    hInterceptY = hIYTile * tileGlobalSize
    hIntercept = (hIX, if dYTile == -1 then hInterceptY + tileGlobalSize else hInterceptY)

--heightNumerator :: Int
--heightNumerator = (tileGlobalSize * scale) `shiftR` 6
--  where
--    -- from wolf source, don't understand what this var is
--    viewGlobal = 0x10000 -- globals visable flush to wall
--    faceDist = round focalLength + minDist
--    scale = fromIntegral halfActionWidth * faceDist `div` (viewGlobal `div` 2)

-- NOTE: This is less efficient than it can be. There's a method involving sin and cos
-- see pg 177
hitDistance :: FineAngle -> (Int, Int) -> (Int, Int) -> Int
hitDistance 0 (x, y) (iX, iY) = max minDist d
  where
    dX = abs (iX - x)
    dY = abs (iY - y)
    dBase = (dX * dX + dY * dY) :: Int
    dSquare = (sqrt (fromIntegral (dBase))) :: Double
    d = round dSquare
hitDistance a (x, y) (iX, iY) = max minDist proposedDist
  where
    boundAngle = if a > fine360 then a - fine360 else if a < 0 then a + fine360 else a
    dY = fromIntegral (abs (iY - y))
    dX = fromIntegral (abs (iX - x))
    d = sqrt (dX * dX + dY * dY)
    proposedDist = round (d * cos (degToRad * fineToNormalAngle boundAngle))
--    proposedDist = abs (round (dX * cosTable!boundAngle - dY * sinTable!boundAngle))
