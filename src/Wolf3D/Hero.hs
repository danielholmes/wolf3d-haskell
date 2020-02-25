module Wolf3D.Hero (
  lastTimeWeaponUsed,
  simUpdateHero,
  staticHeroActionsState,
  createHeroFromTilePosition,
  modifyHeroActionState,
  updateHeroActionsState,
  
  createHero,
  moveHero,
  rotateHero
) where

import Wolf3D.WorldData
import Wolf3D.Geom
import Data.Vector


simUpdateWeapon :: World -> Weapon -> Weapon
simUpdateWeapon w wn
    | isUsingWeapon wn && canUseWeapon (worldTicks w) wn = useWeapon w wn
    | otherwise                                         = wn

canUseWeapon :: WorldTicks -> Weapon -> Bool
canUseWeapon t w = all (< t - timeBetweenUses w) (lastTimeWeaponUsed w)

useWeapon :: World -> Weapon -> Weapon
useWeapon w (Pistol _ s) = Pistol (Just (worldTicks w + 1)) s

lastTimeWeaponUsed :: Weapon -> Maybe WorldTicks
lastTimeWeaponUsed (Pistol t _) = t

timeBetweenUses :: Weapon -> Int
timeBetweenUses (Pistol _ _) = 600

isUsingWeapon :: Weapon -> Bool
isUsingWeapon (Pistol _ s) = s

beingUsed :: Weapon -> Weapon
beingUsed (Pistol t _) = Pistol t True

notBeingUsed :: Weapon -> Weapon
notBeingUsed (Pistol t _) = Pistol t False

{-----------------------------------------------------------------------------------------------------------------------
 Hero
-----------------------------------------------------------------------------------------------------------------------}
staticHeroActionsState :: HeroActionsState
staticHeroActionsState = HeroActionsState False False False False False

modifyHeroActionState :: HeroActionsState -> HeroAction -> Bool -> HeroActionsState
modifyHeroActionState (HeroActionsState _ d l r s) MoveForward a = HeroActionsState a d l r s
modifyHeroActionState (HeroActionsState u _ l r s) MoveBackward a = HeroActionsState u a l r s
modifyHeroActionState (HeroActionsState u d _ r s) TurnLeft a = HeroActionsState u d a r s
modifyHeroActionState (HeroActionsState u d l _ s) TurnRight a = HeroActionsState u d l a s
modifyHeroActionState (HeroActionsState u d l r _) UseWeapon a = HeroActionsState u d l r a

simUpdateHero :: World -> Hero -> Hero
simUpdateHero w h@(Hero {actionsState=has}) = h3
  where
    rotationDelta = heroRotationDelta has
    h1 = rotateHero h rotationDelta

    movementDelta = heroMoveDelta has
    movementScale = if movementDelta < 0 then moveScale else backMoveScale
    -- TODO: set a thrustspeed for AI to use later
    -- thrustspeed += speed;
    velocity = movementScale * movementDelta
    -- TODO
    -- ClipMove(player,xmove,ymove);
    -- player->tilex = player->x >> TILESHIFT;    // scale to tile values
    -- player->tiley = player->y >> TILESHIFT;
    -- offset = farmapylookup[player->tiley]+player->tilex;
    -- player->areanumber = *(mapsegs[0] + offset) -AREATILE;
    h2 = moveHero h1 velocity
    h3 = updateWeapon w h2

angleScale :: Int
angleScale = 20

moveScale :: Int
moveScale = 150

backMoveScale :: Int
backMoveScale = 100

heroBaseMovePerFrame :: Int
heroBaseMovePerFrame = 35

updateWeapon :: World -> Hero -> Hero
updateWeapon w h@(Hero {actionsState=has, weapon=wn}) = h { weapon = simUpdateWeapon w (updateWeaponUsed has wn) }

updateWeaponUsed :: HeroActionsState -> Weapon -> Weapon
updateWeaponUsed has w
  | heroActionsStateUseWeapon has && not current   = beingUsed w
  | not (heroActionsStateUseWeapon has) && current = notBeingUsed w
  | otherwise                                      = w
  where current = isUsingWeapon w

createHero :: Vector2 -> Hero
createHero pos = Hero pos 0 0 staticHeroActionsState (Pistol Nothing False)

createHeroFromTilePosition :: TileCoord -> Hero
createHeroFromTilePosition p = createHero (tileCoordToCentreGlobalPos p)

moveHero :: Hero -> Int -> Hero
moveHero h 0 = h
moveHero h@(Hero {position=p, snappedRotation=sr}) velocity = h {position=newPos}
  where
    speed = abs velocity
    moveAngle = if velocity < 0 then sr else bindAngle (sr + (angles `div` 2))
    boundSpeed = if speed >= minDist * 2 then minDist * 2 - 1 else speed
    rotRad = (fromIntegral moveAngle) * degToRad
    dSpeed = fromIntegral boundSpeed
    newPos = p + Vector2 (dSpeed * (cos rotRad)) (-(dSpeed * (sin rotRad)))

updateHeroActionsState :: HeroActionsState -> Hero -> Hero
updateHeroActionsState a (Hero p sr rr _ w) = Hero p sr rr a w

heroMoveDelta :: HeroActionsState -> Int
heroMoveDelta s = forwardMovement + backwardMovement
  where
    forwardMovement = if heroActionsStateMoveForward s then (-heroBaseMovePerFrame) else 0
    backwardMovement = if heroActionsStateMoveBackward s then heroBaseMovePerFrame else 0

heroRotationDelta :: HeroActionsState -> Int
heroRotationDelta has = leftRotation + rightRotation
  where
    leftRotation = if heroActionsStateTurnLeft has then (-heroBaseMovePerFrame) else 0
    rightRotation = if heroActionsStateTurnRight has then heroBaseMovePerFrame else 0

-- Bound to 0-360
rotateHero :: Hero -> Int -> Hero
rotateHero h@(Hero {snappedRotation=sr, rotationRemainder=rr}) d = h {snappedRotation=(bindAngle newAngle), rotationRemainder=newRealRot}
  where
    newRotDelta = rr + d
    angleUnits = newRotDelta `div` angleScale
    newRealRot = newRotDelta - (angleUnits * angleScale)
    newAngle = sr - angleUnits
