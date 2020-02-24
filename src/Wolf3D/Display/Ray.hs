module Wolf3D.Display.Ray (
  castRayToWalls,
  castRaysToWalls,
  HitDirection (..),
  WallRayHit (..)
) where

import Data.Vector
import Wolf3D.Engine
import Wolf3D.Sim
import Wolf3D.Geom
import Data.Bits
import Data.Array
import Wolf3D.Display.Data

-- Taken from original source, don't know exact meaning yet
focalLength :: Double
focalLength = 0x5700

allFineAngles :: [FineAngle]
allFineAngles = [0..(fineAngles - 1)]

sinTable :: Array Angle Double
sinTable = array (0, fineAngles - 1) [(i, sin (fineToNormalAngle i * degToRad)) | i <- allFineAngles]

focalSinTable :: Array Angle Double
focalSinTable = listArray (0, fineAngles - 1) (map (* focalLength) (elems sinTable))

cosTable :: Array Angle Double
cosTable = array (0, fineAngles - 1) [(i, cos (fineToNormalAngle i * degToRad)) | i <- allFineAngles]

focalCosTable :: Array Angle Double
focalCosTable = listArray (0, fineAngles - 1) (map (* focalLength) (elems cosTable))

tanTable :: Array FineAngle Double
tanTable = array (0, fineAngles - 1) [(i, tan (fineToNormalAngle i * degToRad)) | i <- allFineAngles]

-- TODO: checks for double hor/ver remove need for separate  hor/ver data. Trial it
data RayData = DiagonalRayData {focal :: (Int, Int)
                                , fineMidAngle :: FineAngle
                                , horInterceptYTile :: Int
                                , verInterceptXTile :: Int
                                , xTileStep :: Int
                                , yTileStep :: Int
                                , xStep :: Int
                                , yStep :: Int
                                , horNextIntercept :: (Int, Int)
                                , verNextIntercept :: (Int, Int)} |
                HorizontalRayData {focal :: (Int, Int)
                                  , fineMidAngle :: FineAngle
                                  , interceptY :: Int
                                  , interceptXTile :: Int
                                  , tileStep :: Int} |
                VerticalRayData {focal :: (Int, Int)
                                , fineMidAngle :: FineAngle
                                , interceptX :: Int
                                , interceptYTile :: Int
                                , tileStep :: Int}
  deriving (Show)

fineViewAngleOffsets :: [FineAngle]
fineViewAngleOffsets = reverse (map (*3) [-halfWidth..(halfWidth - 1)])
  where halfWidth = fromIntegral halfActionWidth

fine270 :: Int
fine270 = normalToFineAngle 270

fine90 :: Int
fine90 = normalToFineAngle 90

fine180 :: Int
fine180 = normalToFineAngle 180

fine360 :: Int
fine360 = normalToFineAngle 360

castRaysToWalls :: WallMap -> Vector2 -> FineAngle -> [WallRayHit]
castRaysToWalls wm pos fMA = map (\a -> castRayToWalls wm pos fMA a) rayAngles
  where
    rayAngles = map (\i -> fMA + i) fineViewAngleOffsets

castRayToWalls :: WallMap -> Vector2 -> FineAngle -> FineAngle -> WallRayHit
castRayToWalls wm pos fMA fineViewAngle = nextRayCheck wm (focal rd) rd
  where rd = createRayData pos fMA fineViewAngle

createRayData :: Vector2 -> FineAngle -> FineAngle -> RayData
createRayData pos@(Vector2 x y) fMA fineViewAngle
  | fineViewAngle == 0 || fineViewAngle == fine180  = HorizontalRayData {focal=focal1
                                                              , fineMidAngle=fMA
                                                              , interceptY=focalYI
                                                              , interceptXTile=verInterceptXTile1
                                                              , tileStep=xTileStep1}
  | fineViewAngle == fine90 || fineViewAngle == fine270 = VerticalRayData {focal=focal1
                                                            , fineMidAngle=fMA
                                                            , interceptX=focalXI
                                                            , interceptYTile=horInterceptYTile1
                                                            , tileStep=yTileStep1}
  | is0To89     = let
                    tanTheta = tanTable!fineViewAngle
                    -- aX = focalXI + round (fromIntegral (focalYI - focalTileY * tileGlobalSize) * tanTheta)
                    aX = focalXI + yPartial
                    -- TODO: Use focalXI shift instead, or partial?
                    bYX = fromIntegral (focalXI - (focalTileX * tileGlobalSize))
                    bY = focalYI - round (bYX / tanTheta)
                  in
                    DiagonalRayData {focal=focal1
                                    , fineMidAngle=fMA
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
                                    , fineMidAngle=fMA
                                    , horNextIntercept=(aX, (horInterceptYTile1 + 1) * tileGlobalSize)
                                    , verNextIntercept=(focalTileX * tileGlobalSize, bY)
                                    , horInterceptYTile=horInterceptYTile1
                                    , verInterceptXTile=focalTileX + xTileStep1
                                    , xTileStep=xTileStep1
                                    , yTileStep=yTileStep1
                                    , xStep=(-round (fromIntegral tileGlobalSize * tanTheta))
                                    , yStep=(-round (fromIntegral tileGlobalSize / tanTheta))}
  | is180To269  = let
                    tanTheta = tanTable!(fine270 - fineViewAngle)
                    aX = focalXI - round (fromIntegral (((focalTileY + yTileStep1) * tileGlobalSize) - focalYI) * tanTheta)
                    -- TODO: Use focalXI shift instead
                    bYX = fromIntegral (focalXI - (focalTileX * tileGlobalSize))
                    bY = focalYI + round (bYX / tanTheta)
                  in
                    DiagonalRayData {focal=focal1
                                    , fineMidAngle=fMA
                                    , horNextIntercept=(aX, horInterceptYTile1 * tileGlobalSize)
                                    , verNextIntercept=(focalTileX * tileGlobalSize, bY)
                                    , horInterceptYTile=horInterceptYTile1
                                    , verInterceptXTile=focalTileX + xTileStep1
                                    , xTileStep=xTileStep1
                                    , yTileStep=yTileStep1
                                    , xStep=(- round (fromIntegral tileGlobalSize * tanTheta))
                                    , yStep=(round (fromIntegral tileGlobalSize * tanTable!(fineViewAngle - fine180)))}
  | is270To359  = let
                    tanTheta = tanTable!(fineViewAngle - fine270)
                    aX = focalXI + round (fromIntegral (((focalTileY + yTileStep1) * tileGlobalSize) - focalYI) * tanTheta)
                    bY = focalYI + round (fromIntegral (((focalTileX + xTileStep1) * tileGlobalSize) - focalXI) / tanTheta)
                  in
                    DiagonalRayData {focal=focal1
                                    , fineMidAngle=fMA
                                    , horNextIntercept=(aX, horInterceptYTile1 * tileGlobalSize)
                                    , verNextIntercept=((focalTileX + xTileStep1) * tileGlobalSize, bY)
                                    , horInterceptYTile=horInterceptYTile1
                                    , verInterceptXTile=focalTileX + xTileStep1
                                    , xTileStep=xTileStep1
                                    , yTileStep=yTileStep1
                                    , xStep=(round (fromIntegral tileGlobalSize * tanTheta))
                                    , yStep=(round (fromIntegral tileGlobalSize * tanTable!(fine360 - fineViewAngle)))}
  | fineViewAngle < 0                   = createRayData pos fMA (fineViewAngle + fine360)
  | fineViewAngle >= fine360            = createRayData pos fMA (fineViewAngle - fine360)
  | otherwise                           = error "Shouldn't be able to match this"
  where
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
    yPartialDown = focalYI .&. (tileGlobalSize - 1)
    yPartialUp = tileGlobalSize - yPartialDown

    -- Taken directly from source, find a better way
    is0To89 = fineViewAngle >= 0 && fineViewAngle < fine90
    is90To179 = fineViewAngle >= fine90 && fineViewAngle < fine180
    is180To269 = fineViewAngle >= fine180 && fineViewAngle < fine270
    is270To359 = fineViewAngle >= fine270 && fineViewAngle < fine360

    xTileStep1 = if is0To89 || is270To359 then 1 else -1 :: Int
    yTileStep1 = if is180To269 || is270To359 then 1 else -1 :: Int

--    xPartial = if is0To89 || is270To359 then xPartialUp else xPartialDown
    yPartial = if is180To269 || is270To359 then yPartialUp else yPartialDown

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
                                  , fineMidAngle=fMA
                                  , verInterceptXTile=vIXTile
                                  , xTileStep=xTileS
                                  , yStep=dy
                                  , verNextIntercept=currentIntercept@(x, y)} = case hitting of
    Nothing -> nextRayCheck wm currentIntercept nextD
    Just m  -> WallRayHit {material=m
                          , tilePosition=y - vIYTile * tileGlobalSize
                          , direction=Vertical
                          , distance=hitDistance fMA f currentIntercept
                          , intercept=currentIntercept}
  where
    vIYTile = (y - 1) `shiftR` tileToGlobalShift
    -- intXTile = if xTileS == -1 then vIXTile + 1 else vIXTile
    hitting = wm!!vIXTile!!vIYTile
    -- vIntercept = (intXTile * tileGlobalSize, vIY)

    vInterceptNext = (x + xTileS * tileGlobalSize, y + dy)
    nextD = d{verInterceptXTile=(vIXTile + xTileS)
              , verNextIntercept=vInterceptNext}

verRayCheck wm d@HorizontalRayData {focal=f
                                   , fineMidAngle=fMA
                                   , interceptY=vIY
                                   , interceptXTile=vIXTile
                                   , tileStep=xTileS} = case hitting of
    Nothing -> verRayCheck wm d{interceptXTile=(vIXTile + xTileS)}
    Just m  -> WallRayHit {material=m
                          , tilePosition=vIY - vIYTile * tileGlobalSize
                          , direction=Vertical
                          , distance=hitDistance fMA f vIntercept
                          , intercept=vIntercept}
  where
    vIYTile = (vIY - 1) `shiftR` tileToGlobalShift
    hitting = wm!!vIXTile!!vIYTile

    vInterceptX = vIXTile * tileGlobalSize
    vIntercept = (if xTileS == -1 then vInterceptX + tileGlobalSize else vInterceptX, vIY)

horRayCheck :: WallMap -> RayData -> WallRayHit
horRayCheck _ (HorizontalRayData {}) = error "Shouldn't be called"
horRayCheck wm d@DiagonalRayData {focal=f
                                 , fineMidAngle=fMA
                                 , horInterceptYTile=hIYTile
                                 , yTileStep=dYTile
                                 , xStep=dX
                                 , horNextIntercept=currentIntercept@(x, y)} = case hitting of
    Nothing -> nextRayCheck wm currentIntercept nextD
    Just m  -> WallRayHit {material=m
                          , tilePosition=x - hIXTile * tileGlobalSize
                          , direction=Horizontal
                          , distance=hitDistance fMA f currentIntercept
                          , intercept=currentIntercept}
  where
    hIXTile = (x - 1) `shiftR` tileToGlobalShift
    hitting = wm!!hIXTile!!hIYTile

    hInterceptNext = (x + dX, y + dYTile * tileGlobalSize)
    nextD = d{horInterceptYTile=(hIYTile + dYTile)
            , horNextIntercept=hInterceptNext}
horRayCheck wm d@VerticalRayData {focal=f
                                 , fineMidAngle=fMA
                                 , interceptX=hIX
                                 , interceptYTile=hIYTile
                                 , tileStep=dYTile} = case hitting of
    Nothing -> horRayCheck wm d{interceptYTile=(hIYTile + dYTile)}
    Just m  -> WallRayHit {material=m
                          , tilePosition=hIX - hIXTile * tileGlobalSize
                          , direction=Horizontal
                          , distance=hitDistance fMA f hIntercept
                          , intercept=hIntercept}
  where
    hIXTile = (hIX - 1) `shiftR` tileToGlobalShift
    hitting = wm!!hIXTile!!hIYTile

    hInterceptY = hIYTile * tileGlobalSize
    hIntercept = (hIX, if dYTile == -1 then hInterceptY + tileGlobalSize else hInterceptY)

minDist :: Int
minDist = 0x5800

--heightNumerator :: Int
--heightNumerator = (tileGlobalSize * scale) `shiftR` 6
--  where
--    -- from wolf source, don't understand what this var is
--    viewGlobal = 0x10000 -- globals visable flush to wall
--    faceDist = round focalLength + minDist
--    scale = fromIntegral halfActionWidth * faceDist `div` (viewGlobal `div` 2)

hitDistance :: FineAngle -> (Int, Int) -> (Int, Int) -> Int
hitDistance a (x, y) (iX, iY) = max minDist (abs (round (dX * cosTable!a - dY * sinTable!a)))
  where
    dX = fromIntegral (iX - x)
    dY = fromIntegral (iY - y)
