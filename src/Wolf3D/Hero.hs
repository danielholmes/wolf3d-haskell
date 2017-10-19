module Wolf3D.Hero (
  Hero,
  createHero,
  heroPosition,
  heroRotation,
  heroLookRay,
  moveHero,
  rotateHero
) where

import Wolf3D.Geom
import Data.Vector


type Rotation = Double
data Hero = Hero Vector2 Rotation

createHero :: Vector2 -> Hero
createHero pos = Hero pos 0

heroPosition :: Hero -> Vector2
heroPosition (Hero p _) = p

heroRotation :: Hero -> Double
heroRotation (Hero _ r) = r

heroLookRay :: Hero -> Ray
heroLookRay _ = createRay (Vector2 0 0) (Vector2 0 1)

moveHero :: Hero -> Vector2 -> Hero
moveHero (Hero p r) d = Hero (p + d) r

-- TODO: Bound to 0-360
rotateHero :: Hero -> Double -> Hero
rotateHero (Hero p r) d = Hero p (r + d)
