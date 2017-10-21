module Wolf3D.Items (
  ItemType (Drum),
  Item (Item),
  itemRectangle,
  itemHeight,
  itemSize
) where

import Wolf3D.Geom
import Data.Vector


data ItemType = Drum
  deriving (Show, Eq, Ord)

data Item = Item ItemType Vector2

itemSize :: Item -> Vector2
itemSize _ = Vector2 3000 3000

itemHeight :: Double
itemHeight = 3000

halfItemSize :: Item -> Vector2
halfItemSize i = itemSize i *| 0.5

itemRectangle :: Item -> Rectangle
itemRectangle i@(Item _ o) = Rectangle (o - halfItemSize i) (itemSize i)
