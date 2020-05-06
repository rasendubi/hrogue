module Hrogue.Data.Point
  ( Point(..)
  , pointPlus
  ) where

data Point = Point
  { pointX :: !Int
  , pointY :: !Int
  } deriving (Eq, Show)

pointPlus :: Point -> Point -> Point
pointPlus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
