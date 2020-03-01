module Wolf3D.Hero (
  lastTimeWeaponUsed,
  simUpdateHero,
  staticHeroActionsState,
  createHeroFromTilePosition,
  modifyHeroActionState,

  heroSize,
  createHero,
  rotateHero,
  
  moveHero -- for specs only
) where

import Wolf3D.WorldData
import Wolf3D.Geom
import Data.Vector
import Data.Maybe
import Data.List
import Data.Bits


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
staticHeroActionsState = HeroActionsState False False False False False False

modifyHeroActionState :: HeroActionsState -> HeroAction -> Bool -> HeroActionsState
modifyHeroActionState s MoveForward a = s{heroActionsStateMoveForward=a}
modifyHeroActionState s MoveBackward a = s{heroActionsStateMoveBackward=a}
modifyHeroActionState s TurnLeft a = s{heroActionsStateTurnLeft=a}
modifyHeroActionState s TurnRight a = s{heroActionsStateTurnRight=a}
modifyHeroActionState s UseWeapon a = s{heroActionsStateUseWeapon=a}
modifyHeroActionState s Strafe a = s{heroActionsStateStrafe=a}

simUpdateHero :: World -> Hero -> Hero
simUpdateHero _ (Hero {heroState=HeroAttack}) = error "Not implemented"
simUpdateHero w h@(Hero {heroState=HeroDefault}) = pipeline h
  where
    pipeline = (updateWeapon w)
      . (moveHeroByActionsState w)
      . rotateHeroByActionsState
      . (updateFace w)

--    	CheckWeaponChange ();
--    
--    	if ( buttonstate[bt_use] )
--    		Cmd_Use ();
--    
--    	if ( buttonstate[bt_attack] && !buttonheld[bt_attack])
--    		Cmd_Fire ();
--    
--    	ControlMovement (ob);
--    	if (gamestate.victoryflag)		// watching the BJ actor
--    		return;
--    
--    
--    	plux = player->x >> UNSIGNEDSHIFT;			// scale to fit in unsigned
--    	pluy = player->y >> UNSIGNEDSHIFT;
--    	player->tilex = player->x >> TILESHIFT;		// scale to tile values
--    	player->tiley = player->y >> TILESHIFT;

updateFace :: World -> Hero -> Hero
updateFace w h@(Hero{heroFace=HeroFace count frames})
  | newCount > randomNum = h{heroFace=HeroFace 0 newFrame}
  | otherwise            = h{heroFace=HeroFace newCount frames}
  where
    newCount = count + 1
    randomNum = worldTickRandomNum w
    nextRandomNum = worldTickNextRandomNum w
    proposedFrame = nextRandomNum `shiftR` 6
    newFrame = if proposedFrame == 3 then 1 else proposedFrame
-- TODO: If shooting gatling gun, don't change face (assume it is switched to some 
--  strained face elsewehere


moveHeroByActionsState :: World -> Hero -> Hero
moveHeroByActionsState w h@(Hero{actionsState=has}) = moveHero w velocity h
  where
    movementDelta = heroMoveDelta has
    movementScale = if movementDelta < 0 then moveScale else backMoveScale
    -- TODO: set a thrustspeed for AI to use later
    -- thrustspeed += speed;
    velocity = movementScale * movementDelta

rotateHeroByActionsState :: Hero -> Hero
rotateHeroByActionsState h@(Hero{actionsState=has}) = rotateHero h rotationDelta
  where rotationDelta = heroRotationDelta has

angleScale :: Int
angleScale = 20

moveScale :: Int
moveScale = 150

heroSize :: Int
heroSize = minDist

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
createHero pos = Hero
  { heroFace=HeroFace 0 0
   , heroState=HeroDefault
   , position=pos
   , snappedRotation=0
   , rotationRemainder=0
   , actionsState=staticHeroActionsState
   , weapon=(Pistol Nothing False) }

createHeroFromTilePosition :: TileCoord -> Hero
createHeroFromTilePosition p = createHero (tileCoordToCentreGlobalPos p)

moveHero :: World -> Int -> Hero -> Hero
moveHero _ 0 h = h
moveHero w velocity h@(Hero {position=p, snappedRotation=sr}) = h {position=fromJust firstOkay}
  where
    speed = abs velocity
    moveAngle = if velocity < 0 then sr else bindAngle (sr + (angles `div` 2))
    boundSpeed = if speed >= minDist * 2 then minDist * 2 - 1 else speed
    rotRad = (fromIntegral moveAngle) * degToRad
    dSpeed = fromIntegral boundSpeed
    xOffset = dSpeed * (cos rotRad)
    yOffset = -(dSpeed * (sin rotRad))
    potentialOffsets = [Vector2 xOffset yOffset, Vector2 xOffset 0, Vector2 0 yOffset, Vector2 0 0]
    potentialPositions = map (p +) potentialOffsets
    firstOkay = find (\pos -> not (isHittingWall w pos heroSize)) potentialPositions
    -- TODO
    -- player->tilex = player->x >> TILESHIFT;    // scale to tile values
    -- player->tiley = player->y >> TILESHIFT;
    -- offset = farmapylookup[player->tiley]+player->tilex;
    -- player->areanumber = *(mapsegs[0] + offset) -AREATILE;  

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
