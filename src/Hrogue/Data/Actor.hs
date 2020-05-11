{-# LANGUAGE TemplateHaskell #-}
module Hrogue.Data.Actor
  ( Actor(..)
  , ActorId(..)
  , HasActor(..)
  ) where

import Control.Lens.TH (makeClassy)

import qualified System.Console.ANSI as ANSI

import           Hrogue.Data.Point   (Point)

import Prelude hiding (id)

newtype ActorId = ActorId { unActorId :: Int }
  deriving (Eq, Ord, Show)

data Actor = Actor
    { _actorId   :: !ActorId
    , _position  :: !Point
    , _symbol    :: !Char
    , _sgr       :: ![ANSI.SGR]
    , _hitpoints :: !Int
    }
    deriving (Show)

makeClassy ''Actor
