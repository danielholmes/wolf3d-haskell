module Wolf3D.World (
  createWorld,
  incWorldTicks,
  emptyWallMap,
  tickWorld,
  tickWorldNTimes,

  worldHero,
  worldHeroWeapon,
  worldEnvItemsTouching,
  updateWorldHeroActionsState,

  itemRectangle
) where

import Wolf3D.Geom
import Wolf3D.WorldData
import Wolf3D.Hero
import Data.Vector
import Data.Array


emptyWallMap :: Int -> Int -> WallMap
emptyWallMap w h = listArray ((0, 0), (w - 1, h - 1)) (replicate (w * h) Nothing)

createWorld :: Ceiling -> WallMap -> Hero -> [EnvItem] -> World
createWorld c wm h is = World c wm h is 0

tickWorld :: World -> World
tickWorld world@(World c wm h is t) = incWorldTicks updatedWorld
  where
    newHero = simUpdateHero world h
    updatedWorld = World c wm newHero is t

tickWorldNTimes :: World -> Int -> Maybe World
tickWorldNTimes w n
  | n == 0    = Nothing
  | otherwise = Just (foldr foldStep w [1..n])
    where
      foldStep :: Int -> World -> World
      foldStep _ = tickWorld

incWorldTicks :: World -> World
incWorldTicks (World c wm h is ticks) = World c wm h is (ticks + 1)

updateWorldHeroActionsState :: World -> HeroActionsState -> World
updateWorldHeroActionsState w a = updateWorldHero w (\h -> h {actionsState=a})

updateWorldHero :: World -> (Hero -> Hero) -> World
updateWorldHero (World c wm h is t) op = World c wm (op h) is t

worldEnvItemsTouching :: Rectangle -> World -> [EnvItem]
worldEnvItemsTouching r w = filter (itemIsTouching r) (worldEnvItems w)

itemIsTouching :: Rectangle -> EnvItem -> Bool
itemIsTouching r i = rectangleOverlapsRectangle r (itemRectangle i)

itemRectangle :: EnvItem -> Rectangle
itemRectangle i@(EnvItem _ o) = Rectangle (o - halfItemSize i) (itemSize i)
  where
    -- 0.005 a hack until change rendering
    itemSize :: EnvItem -> Vector2
    itemSize _ = Vector2 ((fromIntegral tileGlobalSize) * 0.05) ((fromIntegral tileGlobalSize) * 0.05)

    halfItemSize :: EnvItem -> Vector2
    halfItemSize iS = itemSize iS *| 0.5
