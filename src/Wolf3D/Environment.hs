module Wolf3D.Environment (
  EnvItemType (Drum, Flag, Light),
  EnvItem (EnvItem),
  itemRectangle,
  itemHeight,
  itemSize
) where

import Wolf3D.Geom
import Wolf3D.Sim
import Data.Vector


data EnvItemType = Drum | Flag | Light
  deriving (Show, Eq, Ord)

data EnvItem = EnvItem EnvItemType Vector2
  deriving (Show, Eq)

instance SimItem EnvItem where
  simUpdate _ = id

itemSize :: EnvItem -> Vector2
itemSize _ = Vector2 3000 3000

itemHeight :: Double
itemHeight = 3000

halfItemSize :: EnvItem -> Vector2
halfItemSize i = itemSize i *| 0.5

itemRectangle :: EnvItem -> Rectangle
itemRectangle i@(EnvItem _ o) = Rectangle (o - halfItemSize i) (itemSize i)
