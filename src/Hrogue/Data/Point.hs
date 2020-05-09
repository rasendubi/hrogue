module Hrogue.Data.Point
  ( Point(..)
  , pointPlus
  , pointMinus
  ) where

data Point = Point
  { pointX :: !Int
  , pointY :: !Int
  } deriving (Eq, Ord, Show)

pointPlus :: Point -> Point -> Point
pointPlus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

pointMinus :: Point -> Point -> Point
pointMinus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

instance Semigroup Point where
  (<>) = pointPlus

instance Monoid Point where
  mempty = Point 0 0
