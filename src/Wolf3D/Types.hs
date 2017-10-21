module Wolf3D.Types (
  PosInt,
  PosZInt,
  PosZDouble,
  PosDouble,
  posZInt,
  posZInt0,
  posInt,
  posDouble,
  fromPosDouble,
  fromPosInt,
  fromPosZInt,
  posZDouble,
  fromPosZDouble
) where


newtype PosZInt = PosZInt Int
  deriving (Show, Eq, Ord)

newtype PosInt = PosInt Int
  deriving (Show, Eq, Ord)

newtype PosZDouble = PosZDouble Double
  deriving (Show, Eq, Ord)

newtype PosDouble = PosDouble Double
  deriving (Show, Eq, Ord)

posZInt0 :: PosZInt
posZInt0 = posZInt 0

posZInt :: Int -> PosZInt
posZInt r
  | r >= 0    = PosZInt r
  | otherwise = error ("Invalid posZInt " ++ show r)

fromPosZInt :: PosZInt -> Int
fromPosZInt (PosZInt i) = i

posInt :: Int -> PosInt
posInt r
  | r >= 1    = PosInt r
  | otherwise = error ("Invalid posInt " ++ show r)

fromPosInt :: PosInt -> Int
fromPosInt (PosInt i) = i

posZDouble :: Double -> PosZDouble
posZDouble r
  | r >= 0    = PosZDouble r
  | otherwise = error ("Invalid PosZDouble " ++ show r)

fromPosZDouble :: PosZDouble -> Double
fromPosZDouble (PosZDouble i) = i

posDouble :: Double -> PosDouble
posDouble r
  | r >= 0    = PosDouble r
  | otherwise = error ("Invalid PosDouble " ++ show r)

fromPosDouble :: PosDouble -> Double
fromPosDouble (PosDouble i) = i
