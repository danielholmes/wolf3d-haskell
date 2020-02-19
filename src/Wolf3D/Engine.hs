{-# LANGUAGE GADTs #-}
module Wolf3D.Engine (
  SimEntity (simUpdate),
  World,
  WorldTicks,
  WallMap,
  Wall (Wall),
  WallMaterial (Red, Green, Blue, Blue2, Blue3, Blue4),
  WallHit (WallHit),
  TileCoord,
  Ceiling (GreyCeiling, GreenCeiling, PurpleCeiling, YellowCeiling),
  createWorld,
  worldWalls,
  worldEntities,
  updateWorldEntities,
  ticWorldTicks,
  worldWallsTouching,
  wallToLine,
  castRayToClosestWall,
  wallHeight,
  emptyWallMap,
  worldTics,
  worldCeilingColor,
  worldWallMap,
  tickWorld,
  tickWorldNTimes,

  tileCoordToGlobalPos,
  tileCoordToCentreGlobalPos,
  tileGlobalSize
) where

import Wolf3D.Geom
import Data.Vector
import Data.List
import Data.Bits


class SimEntity i where
  simUpdate :: World a -> i -> i

-- TODO: See if way of moving wall materials outside of engine
data WallMaterial = Red | Green | Blue | Blue2 | Blue3 | Blue4
  deriving (Show, Eq, Ord)

type TileCoord = (Int, Int)
type WallPosition = Vector2
type WallSize = Vector2
data Wall = Wall WallPosition WallSize WallMaterial
  deriving (Show, Eq)

type DistanceToWall = Double
type HitPosition = Vector2
data WallHit = WallHit Wall HitPosition DistanceToWall
  deriving (Show, Eq)

data Ceiling = GreyCeiling | PurpleCeiling | GreenCeiling | YellowCeiling
  deriving (Show, Eq, Ord)

type WorldTicks = Int
type WallMap = [[Maybe WallMaterial]]
-- Tried record syntax for this and failed
data World i where
  World :: (SimEntity i) => Ceiling -> [Wall] -> WallMap -> [i] -> WorldTicks -> World i

tileGlobalSize :: Int
tileGlobalSize = 1 `shiftL` 16

tileToGlobalShift :: Int
tileToGlobalShift =  16

tileCentreGlobalOffset :: Vector2
tileCentreGlobalOffset = Vector2 (fromIntegral (tileGlobalSize `div` 2)) (fromIntegral (tileGlobalSize `div` 2))

tileCoordToCentreGlobalPos :: TileCoord -> Vector2
tileCoordToCentreGlobalPos p = (tileCoordToGlobalPos p) + tileCentreGlobalOffset

tileCoordToGlobalPos :: TileCoord -> Vector2
tileCoordToGlobalPos (tileX, tileY) = Vector2 (fromIntegral worldX) (fromIntegral worldY)
  where
    worldX = tileX `shiftL` tileToGlobalShift
    worldY = tileY `shiftL` tileToGlobalShift
    -- #define GLOBAL1    (1l<<16)
    -- #define TILEGLOBAL  GLOBAL1
    -- #define PIXGLOBAL  (GLOBAL1/64)
    -- #define TILESHIFT    16l
    --  SpawnPlayer(x,y,NORTH+tile-19);
    -- player->obclass = playerobj;
    -- player->active = true;
    -- player->tilex = tilex;
    -- player->tiley = tiley;
    -- player->areanumber =
    --        *(mapsegs[0] + farmapylookup[player->tiley]+player->tilex);
    -- player->x = ((long)tilex<<TILESHIFT)+TILEGLOBAL/2;
    -- player->y = ((long)tiley<<TILESHIFT)+TILEGLOBAL/2;

--globalSizeToTileCoord :: Double -> Int
--globalSizeToTileCoord s = (round s) `shiftR` tileToGlobalShift
--
--globalPosToTileCoord :: Vector2 -> TileCoord
--globalPosToTileCoord (Vector2 x y) = (globalSizeToTileCoord x, globalSizeToTileCoord y)

emptyWallMap :: Int -> Int -> WallMap
emptyWallMap w h = replicate h $ replicate w Nothing

createWorld :: (SimEntity i) => Ceiling -> WallMap -> [i] -> World i
createWorld c wm is = World c ws wm is 0
  where
    ws = concat (map (\(x, col) -> concat (map (\(y, cell) -> createWalls (x, y) cell) (zip [0..] col))) (zip [0..] wm))
    
    createWalls :: TileCoord -> Maybe WallMaterial -> [Wall]
    createWalls _ Nothing = []
    createWalls pos@(x, y) (Just m) = [top, right, bottom, left]
      where
        top = Wall (tileCoordToGlobalPos pos) (tileCoordToGlobalPos (1, 0)) m
        right = Wall (tileCoordToGlobalPos (x + 1, y)) (tileCoordToGlobalPos (0, 1)) m
        bottom = Wall (tileCoordToGlobalPos (x, y + 1)) (tileCoordToGlobalPos (1, 0)) m
        left = Wall (tileCoordToGlobalPos pos) (tileCoordToGlobalPos (0, 1)) m

tickWorld :: World i -> World i
tickWorld world@(World _ _ _ is _) = ticWorldTicks updatedWorld
  where
    updatedItems = map (simUpdate world) is
    updatedWorld = updateWorldEntities world updatedItems

tickWorldNTimes :: World i -> Int -> Maybe (World i)
tickWorldNTimes w n
  | n == 0    = Nothing
  | otherwise = Just (foldr foldStep w [1..n])
    where
      foldStep :: Int -> World i -> World i
      foldStep _ = tickWorld

updateWorldEntities :: World i -> [i] -> World i
updateWorldEntities (World c w wm _ t) i = World c w wm i t

worldWalls :: World i -> [Wall]
worldWalls (World _ ws _ _ _) = ws

worldWallMap :: World i -> WallMap
worldWallMap (World _ _ wm _ _) = wm

worldEntities :: (SimEntity i) => World i -> [i]
worldEntities (World _ _ _ is _) = is

worldCeilingColor :: World i -> Ceiling
worldCeilingColor (World c _ _ _ _) = c

worldTics :: World i -> Int
worldTics (World _ _ _ _ t) = t

-- TODO: Remove, was used in mini map
worldWallsTouching :: World i -> Rectangle -> [Wall]
worldWallsTouching w r = filter (wallIsTouching r) (worldWalls w)

wallIsTouching :: Rectangle -> Wall -> Bool
wallIsTouching r w = rectangleTouchesLine r (wallToLine w)

wallToLine :: Wall -> Line
wallToLine (Wall start change _) = (start, change)

wallHeight :: Double
wallHeight = 3000

ticWorldTicks :: World i -> World i
ticWorldTicks (World c ws wm is ticks) = World c ws wm is (ticks + 1)

castRayToClosestWall :: World i -> Ray -> Maybe WallHit
castRayToClosestWall w ray
  | null allHits = Nothing
  | otherwise    = Just (minimumBy compareHits allHits)
  where
    allHits = castRayToAllWalls w ray
    compareHits :: WallHit -> WallHit -> Ordering
    compareHits (WallHit _ _ d1) (WallHit _ _ d2) = d1 `compare` d2

castRayToAllWalls :: World i -> Ray -> [WallHit]
castRayToAllWalls world ray = foldr foldStep [] (worldWalls world)
  where
    rStart = rayOrigin ray
    foldStep :: Wall -> [WallHit] -> [WallHit]
    foldStep wall accu = case rayLineIntersection ray (wallToLine wall) of
      Nothing -> accu
      Just pos -> WallHit wall pos (vectorDist rStart pos) : accu
