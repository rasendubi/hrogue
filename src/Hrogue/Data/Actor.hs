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
    { actorId        :: !ActorId
    , actorPosition  :: !Point
    , actorSymbol    :: !Char
    , actorSgr       :: ![ANSI.SGR]
    , actorHitpoints :: !Int
    , actorState     :: !state
    }

deriving instance Show state => Show (Actor state)
