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
heroLookRay (Hero pos rot) = createRay pos (Vector2 (sin rot) (cos rot))

moveHero :: Hero -> Double -> Hero
moveHero (Hero p r) m = Hero (p + (m |* angleToVector2 r)) r

-- TODO: Bound to (-pi) - pi
rotateHero :: Hero -> Double -> Hero
rotateHero (Hero p r) d = Hero p (r + d)
