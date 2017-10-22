module Wolf3D.Wolf3DSim (
  Wolf3DSimItem (SIEnvItem, SIHero),
  worldHero,
  worldHeroWeapon,
  worldEnvItems,
  worldEnvItemsTouching,
  updateWorldHeroActionsState
) where

import Wolf3D.Geom
import Wolf3D.Sim
import Wolf3D.Environment
import Wolf3D.Hero
import Data.Maybe (fromJust)
import Data.List (find)


data Wolf3DSimItem = SIEnvItem EnvItem | SIHero Hero
  deriving (Show, Eq)

instance SimItem Wolf3DSimItem where
  simUpdate t (SIEnvItem i) = SIEnvItem (simUpdate t i)
  simUpdate t (SIHero i) = SIHero (simUpdate t i)

worldHero :: World Wolf3DSimItem -> Hero
worldHero w = fromJust (fmap (\(SIHero h) -> h) (find (\i -> case i of (SIHero _) -> True; _ -> False) (worldItems w)))

worldHeroWeapon :: World Wolf3DSimItem -> Weapon
worldHeroWeapon = heroWeapon . worldHero

worldEnvItems :: World Wolf3DSimItem -> [EnvItem]
worldEnvItems w = map (\(SIEnvItem e) -> e) (filter (\i -> case i of (SIEnvItem _) -> True; _ -> False) (worldItems w))

updateWorldHeroActionsState :: World Wolf3DSimItem -> HeroActionsState -> World Wolf3DSimItem
updateWorldHeroActionsState w a = updateWorldHero w (\h -> updateHeroActionsState h a)

updateWorldHero :: World Wolf3DSimItem -> (Hero -> Hero) -> World Wolf3DSimItem
updateWorldHero w op = updateWorldItems w newItems
  where
    newItems = foldr foldStep [] (worldItems w)
    foldStep :: Wolf3DSimItem -> [Wolf3DSimItem] -> [Wolf3DSimItem]
    foldStep (SIHero h) accu = SIHero (op h) : accu
    foldStep i accu = i : accu

worldEnvItemsTouching :: Rectangle -> World Wolf3DSimItem -> [EnvItem]
worldEnvItemsTouching r w = filter (itemIsTouching r) (worldEnvItems w)

itemIsTouching :: Rectangle -> EnvItem -> Bool
itemIsTouching r i = rectangleOverlapsRectangle r (itemRectangle i)
