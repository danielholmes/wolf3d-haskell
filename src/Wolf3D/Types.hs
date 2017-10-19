module Wolf3D.Types (
  PosInt,
  PosZInt,
  PosZDouble,
  posZInt,
  posZInt0,
  posInt,
  fromPosInt,
  fromPosZInt,
  posZDouble,
  fromPosZDouble
) where

import Data.Vector


newtype PosZInt = PosZInt Int

instance Eq PosZInt where
  (==) (PosZInt i1) (PosZInt i2) = i1 == i2

instance Show PosZInt where
  show (PosZInt i) = show i

instance Ord PosZInt where
  (PosZInt i1) `compare` (PosZInt i2) = i1 `compare` i2


newtype PosInt = PosInt Int

instance Eq PosInt where
  (==) (PosInt i1) (PosInt i2) = i1 == i2

instance Show PosInt where
  show (PosInt i) = show i

instance Ord PosInt where
  (PosInt i1) `compare` (PosInt i2) = i1 `compare` i2


newtype PosZDouble = PosZDouble Double

instance Eq PosZDouble where
  (==) (PosZDouble i1) (PosZDouble i2) = i1 == i2

instance Show PosZDouble where
  show (PosZDouble i) = show i

instance Ord PosZDouble where
  (PosZDouble i1) `compare` (PosZDouble i2) = i1 `compare` i2


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
