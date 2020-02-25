module Wolf3D.Debug.Dummy (dummyWorld) where

import Wolf3D.Engine
import Wolf3D.Sim
import Wolf3D.DataHelpers

dummyWorld :: World Wolf3DSimEntity
dummyWorld = createWorld GreyCeiling wm [hero]
  where
    wm = visualListToWallMap [[Just Blue1, Just Blue1, Just Grey1, Just Blue1, Just Blue1],
                             [Just Blue1,  Nothing,    Nothing,    Nothing,    Just Blue1],
                             [Just Blue1,  Nothing,    Nothing,    Nothing,    Just Blue1],
                             [Just Blue2,  Nothing,    Nothing,    Nothing,    Just Blue2],
                             [Just Blue2,  Nothing,    Nothing,    Nothing,    Just Blue2],
                             [Just Blue2,  Just Blue2, Just Blue2, Just Blue2, Just Blue2]]
    hero = SEHero (rotateHero (createHeroFromTilePosition (2, 3)) ((-90) * 20))
