{-# LANGUAGE GADTs #-}
module Wolf3D.Engine (
  SimEntity (simUpdate),
  World,
  WorldTicks,
  WallMap,
  WallMaterial (Grey1, Grey2, Blue1, Blue2),
  TileCoord,
  Ceiling (GreyCeiling, GreenCeiling, PurpleCeiling, YellowCeiling),
  createWorld,
  worldEntities,
  updateWorldEntities,
  ticWorldTicks,
  wallHeight,
  emptyWallMap,
  worldTics,
  worldCeilingColor,
  worldWallMap,
  tickWorld,
  tickWorldNTimes,

  tileCoordToGlobalPos,
  tileCoordToCentreGlobalPos,
  tileGlobalSize,
  tileToGlobalShift
) where

import Data.Vector
import Data.Bits


class SimEntity i where
  simUpdate :: World a -> i -> i

-- TODO: See if way of moving wall materials outside of engine
data WallMaterial = Grey1 | Grey2 | Blue1 | Blue2
  deriving (Show, Eq, Ord)

type TileCoord = (Int, Int)

data Ceiling = GreyCeiling | PurpleCeiling | GreenCeiling | YellowCeiling
  deriving (Show, Eq, Ord)

type WorldTicks = Int
type WallMap = [[Maybe WallMaterial]]
-- Tried record syntax for this and failed
data World i where
  World :: (SimEntity i) => Ceiling -> WallMap -> [i] -> WorldTicks -> World i

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

-- TODO: Test throws error if non equal map dims
createWorld :: (SimEntity i) => Ceiling -> WallMap -> [i] -> World i
createWorld c wm is = World c wm is 0

tickWorld :: World i -> World i
tickWorld world@(World _ _ is _) = ticWorldTicks updatedWorld
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
updateWorldEntities (World c wm _ t) i = World c wm i t

worldWallMap :: World i -> WallMap
worldWallMap (World _ wm _ _) = wm

worldEntities :: (SimEntity i) => World i -> [i]
worldEntities (World _ _ is _) = is

worldCeilingColor :: World i -> Ceiling
worldCeilingColor (World c _ _ _) = c

worldTics :: World i -> Int
worldTics (World _ _ _ t) = t

-- deprecated
wallHeight :: Double
wallHeight = 3000

ticWorldTicks :: World i -> World i
ticWorldTicks (World c wm is ticks) = World c wm is (ticks + 1)
