module Wolf3D.Display.Ray (
  castRayToWalls,
  castRaysToWalls,
  HitDirection (Horizontal, Vertical),
  WallRayHit (WallRayHit, material, tilePosition, intercept, direction)
) where

import Data.Vector
import Wolf3D.Engine
import Wolf3D.Sim
import Wolf3D.Geom
import Data.Bits
import Data.Array
import Wolf3D.Display.Data

data HitDirection = Horizontal | Vertical
  deriving (Show, Eq)

data WallRayHit = WallRayHit {material :: WallMaterial
                              , direction :: HitDirection
                              , tilePosition :: Int
                              , intercept :: (Int, Int)}
  deriving (Eq, Show)

-- Taken from original source, don't know exact meaning yet
focalLength :: Double
focalLength = 0x5700

allFineAngles :: [FineAngle]
allFineAngles = [0..(fineAngles - 1)]

focalSinTable :: Array Angle Double
focalSinTable = array (0, fineAngles - 1) [(i, focalLength * sin (fineToNormalAngle i * deg2Rad)) | i <- allFineAngles]

focalCosTable :: Array Angle Double
focalCosTable = array (0, fineAngles - 1) [(i, focalLength * cos (fineToNormalAngle i * deg2Rad)) | i <- allFineAngles]

tanTable :: Array FineAngle Double
tanTable = array (0, fineAngles - 1) [(i, tan (fineToNormalAngle i * deg2Rad)) | i <- allFineAngles]

-- TODO: checks for double hor/ver remove need for separate  hor/ver data. Trial it
data RayData = DiagonalRayData {horInterceptYTile :: Int
                                , verInterceptXTile :: Int
                                , xTileStep :: Int
                                , yTileStep :: Int
                                , xStep :: Int
                                , yStep :: Int
                                , horNextIntercept :: (Int, Int)
                                , verNextIntercept :: (Int, Int)} |
                HorizontalRayData {interceptY :: Int
                                  , interceptXTile :: Int
                                  , tileStep :: Int} |
                VerticalRayData {interceptX :: Int
                                , interceptYTile :: Int
                                , tileStep :: Int}
  deriving (Show)

--fieldOfView :: Double
--fieldOfView = pi / 3

-- Note, needs to be fine angles
fineViewAngleOffsets :: [FineAngle]
fineViewAngleOffsets = [-halfActionWidth..(halfActionWidth - 1)]
  where
    halfActionWidth = fromIntegral actionWidth `div` 2
--  where
--    angleDiff = fieldOfView / fromIntegral actionWidth

fine270 :: Int
fine270 = normalToFineAngle 270

fine90 :: Int
fine90 = normalToFineAngle 90

fine180 :: Int
fine180 = normalToFineAngle 180

fine360 :: Int
fine360 = normalToFineAngle 360

castRaysToWalls :: WallMap -> Vector2 -> FineAngle -> [WallRayHit]
castRaysToWalls wm pos fineMidAngle = map (\a -> castRayToWalls wm pos a) rayAngles
  where
    rayAngles = map (\i -> fineMidAngle + i) fineViewAngleOffsets

castRayToWalls :: WallMap -> Vector2 -> FineAngle -> WallRayHit
castRayToWalls wm pos fineViewAngle = nextRayCheck wm focal rd
  where (focal, rd) = createRayData pos fineViewAngle

createRayData :: Vector2 -> FineAngle -> ((Int, Int), RayData)
createRayData (Vector2 x y) fineViewAngle
  | fineViewAngle == 0 || fineViewAngle == fine180  = (focal
                                                            , HorizontalRayData {interceptY=focalYI
                                                              , interceptXTile=verInterceptXTile1
                                                              , tileStep=xTileStep1})
  | fineViewAngle == fine90 || fineViewAngle == fine270 = (focal
                                                            , VerticalRayData {interceptX=focalXI
                                                            , interceptYTile=horInterceptYTile1
                                                            , tileStep=yTileStep1})
  | is0To89     = let
                    tanTheta = tanTable!fineViewAngle
                    -- aX = focalXI + round (fromIntegral (focalYI - focalTileY * tileGlobalSize) * tanTheta)
                    aX = focalXI + yPartial
                    -- TODO: Use focalXI shift instead, or partial?
                    bYX = fromIntegral (focalXI - (focalTileX * tileGlobalSize))
                    bY = focalYI - round (bYX / tanTheta)
                  in
                    (focal
                    , DiagonalRayData {horNextIntercept=(aX, (horInterceptYTile1 + 1) * tileGlobalSize)
                                     , verNextIntercept=((focalTileX + xTileStep1) * tileGlobalSize, bY)
                                     , horInterceptYTile=horInterceptYTile1
                                     , verInterceptXTile=focalTileX + xTileStep1
                                     , xTileStep=xTileStep1
                                     , yTileStep=yTileStep1
                                     , xStep=(round (fromIntegral tileGlobalSize * tanTable!(fine90 - fineViewAngle)))
                                     , yStep=(-round (fromIntegral tileGlobalSize * tanTheta))})
  | is90To179   = let
                    tanTheta = tanTable!(fineViewAngle - fine90)
                    aY = focalTileY * tileGlobalSize
                    aX = focalXI - round (fromIntegral (focalYI - aY) * tanTheta)
                    -- TODO: Use focalXI shift instead
                    bX = fromIntegral (focalXI - focalTileX * tileGlobalSize)
                    bY = round (focalY - bX / tanTheta)
                  in
                    (focal
                    , DiagonalRayData {horNextIntercept=(aX, (horInterceptYTile1 + 1) * tileGlobalSize)
                                      , verNextIntercept=(focalTileX * tileGlobalSize, bY)
                                      , horInterceptYTile=horInterceptYTile1
                                      , verInterceptXTile=focalTileX + xTileStep1
                                      , xTileStep=xTileStep1
                                      , yTileStep=yTileStep1
                                      , xStep=(-round (fromIntegral tileGlobalSize * tanTheta))
                                      , yStep=(-round (fromIntegral tileGlobalSize / tanTheta))})
  | is180To269  = let
                    tanTheta = tanTable!(fine270 - fineViewAngle)
                    aX = focalXI - round (fromIntegral (((focalTileY + yTileStep1) * tileGlobalSize) - focalYI) * tanTheta)
                    -- TODO: Use focalXI shift instead
                    bYX = fromIntegral (focalXI - (focalTileX * tileGlobalSize))
                    bY = focalYI + round (bYX / tanTheta)
                  in
                    (focal
                    , DiagonalRayData {horNextIntercept=(aX, horInterceptYTile1 * tileGlobalSize)
                                      , verNextIntercept=(focalTileX * tileGlobalSize, bY)
                                      , horInterceptYTile=horInterceptYTile1
                                      , verInterceptXTile=focalTileX + xTileStep1
                                      , xTileStep=xTileStep1
                                      , yTileStep=yTileStep1
                                      , xStep=(- round (fromIntegral tileGlobalSize * tanTheta))
                                      , yStep=(round (fromIntegral tileGlobalSize * tanTable!(fineViewAngle - fine180)))})
  | is270To359  = let
                    tanTheta = tanTable!(fineViewAngle - fine270)
                    aX = focalXI + round (fromIntegral (((focalTileY + yTileStep1) * tileGlobalSize) - focalYI) * tanTheta)
                    bY = focalYI + round (fromIntegral (((focalTileX + xTileStep1) * tileGlobalSize) - focalXI) / tanTheta)
                  in
                    (focal
                    , DiagonalRayData {horNextIntercept=(aX, horInterceptYTile1 * tileGlobalSize)
                                      , verNextIntercept=((focalTileX + xTileStep1) * tileGlobalSize, bY)
                                      , horInterceptYTile=horInterceptYTile1
                                      , verInterceptXTile=focalTileX + xTileStep1
                                      , xTileStep=xTileStep1
                                      , yTileStep=yTileStep1
                                      , xStep=(round (fromIntegral tileGlobalSize * tanTheta))
                                      , yStep=(round (fromIntegral tileGlobalSize * tanTable!(fine360 - fineViewAngle)))})
  | otherwise                           = error "Angle must be >= 0 < 360"
  where
    viewSin = focalSinTable!fineViewAngle
    viewCos = focalCosTable!fineViewAngle
    focalX = x - viewCos
    focalY = y + viewSin
    focalXI :: Int
    focalXI = floor focalX
    focalYI :: Int
    focalYI = floor focalY
    focal = (focalXI, focalYI)
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
nextRayCheck wm _ d@(HorizontalRayData _ _ _) = verRayCheck wm d
nextRayCheck wm _ d@(VerticalRayData _ _ _) = horRayCheck wm d

verRayCheck :: WallMap -> RayData -> WallRayHit
verRayCheck _ (VerticalRayData {}) = error "Shouldn't be called"
verRayCheck wm d@DiagonalRayData {verInterceptXTile=vIXTile
                                  , xTileStep=xTileS
                                  , yStep=dy
                                  , verNextIntercept=currentIntercept@(x, y)} = case hitting of
    Nothing -> nextRayCheck wm currentIntercept nextD
    Just m  -> WallRayHit {material=m
                          , tilePosition=y - vIYTile * tileGlobalSize
                          , direction=Vertical
                          , intercept=currentIntercept}
  where
    vIYTile = (y - 1) `shiftR` tileToGlobalShift
    -- intXTile = if xTileS == -1 then vIXTile + 1 else vIXTile
    hitting = wm!!vIXTile!!vIYTile
    -- vIntercept = (intXTile * tileGlobalSize, vIY)

    vInterceptNext = (x + xTileS * tileGlobalSize, y + dy)
    nextD = d{verInterceptXTile=(vIXTile + xTileS)
              , verNextIntercept=vInterceptNext}

verRayCheck wm d@HorizontalRayData {interceptY=vIY
                                  , interceptXTile=vIXTile
                                  , tileStep=xTileS} = case hitting of
    Nothing -> verRayCheck wm d{interceptXTile=(vIXTile + xTileS)}
    Just m  -> WallRayHit {material=m
                          , tilePosition=vIY - vIYTile * tileGlobalSize
                          , direction=Vertical
                          , intercept=vIntercept}
  where
    vIYTile = (vIY - 1) `shiftR` tileToGlobalShift
    hitting = wm!!vIXTile!!vIYTile

    vInterceptX = vIXTile * tileGlobalSize
    vIntercept = (if xTileS == -1 then vInterceptX + tileGlobalSize else vInterceptX, vIY)

horRayCheck :: WallMap -> RayData -> WallRayHit
horRayCheck _ (HorizontalRayData {}) = error "Shouldn't be called"
horRayCheck wm d@DiagonalRayData {horInterceptYTile=hIYTile
                                 , yTileStep=dYTile
                                 , xStep=dX
                                 , horNextIntercept=currentIntercept@(x, y)} = case hitting of
    Nothing -> nextRayCheck wm currentIntercept nextD
    Just m  -> WallRayHit {material=m
                          , tilePosition=x - hIXTile * tileGlobalSize
                          , direction=Horizontal
                          , intercept=currentIntercept}
  where
    hIXTile = (x - 1) `shiftR` tileToGlobalShift
    hitting = wm!!hIXTile!!hIYTile

    hInterceptNext = (x + dX, y + dYTile * tileGlobalSize)
    nextD = d{horInterceptYTile=(hIYTile + dYTile)
            , horNextIntercept=hInterceptNext}
horRayCheck wm d@VerticalRayData {interceptX=hIX
                                 , interceptYTile=hIYTile
                                 , tileStep=dYTile} = case hitting of
    Nothing -> horRayCheck wm d{interceptYTile=(hIYTile + dYTile)}
    Just m  -> WallRayHit {material=m
                          , tilePosition=hIX - hIXTile * tileGlobalSize
                          , direction=Horizontal
                          , intercept=hIntercept}
  where
    hIXTile = (hIX - 1) `shiftR` tileToGlobalShift
    hitting = wm!!hIXTile!!hIYTile

    hInterceptY = hIYTile * tileGlobalSize
    hIntercept = (hIX, if dYTile == -1 then hInterceptY + tileGlobalSize else hInterceptY)
