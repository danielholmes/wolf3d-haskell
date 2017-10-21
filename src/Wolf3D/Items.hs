module Wolf3D.Items (
  ItemType (Drum),
  Item (Item),
  itemRectangle
) where

import Wolf3D.Geom
import Data.Vector


data ItemType = Drum
  deriving (Show, Eq, Ord)

data Item = Item ItemType Vector2

itemSize :: Vector2
itemSize = Vector2 3000 3000

halfItemSize :: Vector2
halfItemSize = itemSize *| 0.5

itemRectangle :: Item -> Rectangle
itemRectangle (Item _ o) = Rectangle (o - halfItemSize) itemSize
