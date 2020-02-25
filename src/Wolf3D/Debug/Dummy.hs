module Wolf3D.Debug.Dummy (dummyWorld) where

import Wolf3D.WorldData
import Wolf3D.Sim
import Wolf3D.Hero
import Wolf3D.DataHelpers

dummyWorld :: World
dummyWorld = createWorld GreyCeiling wm hero []
  where
    wm = visualListToWallMap [[Just Blue1, Just Blue1, Just Grey1, Just Blue1, Just Blue1],
                             [Just Blue1,  Nothing,    Nothing,    Nothing,    Just Blue1],
                             [Just Blue1,  Nothing,    Nothing,    Nothing,    Just Blue1],
                             [Just Blue2,  Nothing,    Nothing,    Nothing,    Just Blue2],
                             [Just Blue2,  Nothing,    Nothing,    Nothing,    Just Blue2],
                             [Just Blue2,  Just Blue2, Just Blue2, Just Blue2, Just Blue2]]
    hero = rotateHero (createHeroFromTilePosition (2, 3)) ((-90) * 20)
