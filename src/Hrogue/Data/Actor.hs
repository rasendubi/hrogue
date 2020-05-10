{-# LANGUAGE StandaloneDeriving #-}
module Hrogue.Data.Actor
  ( Actor(..)
  , ActorId(..)
  ) where

import qualified System.Console.ANSI as ANSI

import           Hrogue.Data.Point   (Point)

newtype ActorId = ActorId { unActorId :: Int }
  deriving (Eq, Ord, Show)

data Actor state = Actor
    { id        :: !ActorId
    , position  :: !Point
    , symbol    :: !Char
    , sgr       :: ![ANSI.SGR]
    , hitpoints :: !Int
    , state     :: !state
    }

deriving instance Show state => Show (Actor state)
