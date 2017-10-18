module Wolf3D.Hero (
  Hero,
  createHero,
  heroPosition,
  moveHero
) where

import Data.Vector


type Rotation = Double
data Hero = Hero Vector2 Rotation

createHero :: Vector2 -> Hero
createHero pos = Hero pos 0

heroPosition :: Hero -> Vector2
heroPosition (Hero p _) = p

moveHero :: Hero -> Vector2 -> Hero
moveHero (Hero pos r) d = Hero (pos + d) r
