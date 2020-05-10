module Hrogue.Data.Point
  ( Point(..)
  , pointPlus
  , pointMinus
  , left, right, up, down
  , directions
  ) where

data Point = Point
    { pointX :: !Int
    , pointY :: !Int
    }
    deriving (Eq, Ord, Show)

instance Semigroup Point where
  (<>) = pointPlus

instance Monoid Point where
  mempty = Point 0 0

pointPlus :: Point -> Point -> Point
pointPlus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

pointMinus :: Point -> Point -> Point
pointMinus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

left, right, up, down :: Point
left  = Point (-1) 0
right = Point 1 0
up    = Point 0 (-1)
down  = Point 0 1

directions :: [Point]
directions =
  [ left
  , right
  , up
  , down
  , left <> up
  , left <> down
  , right <> up
  , right <> down
  ]
