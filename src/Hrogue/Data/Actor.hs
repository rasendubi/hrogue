{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Hrogue.Data.Actor
  ( Actor(..)
  , ActorId(..)
  , BaseActor(..)
  , HasBaseActor(..)
  ) where

import           Control.Lens.TH     (makeClassy)

import qualified Data.Text           as T

import qualified System.Console.ANSI as ANSI

import           Hrogue.Data.Point   (Point)

class Actor action actor where
  takeTurn :: actor -> action

newtype ActorId = ActorId { unActorId :: Int }
  deriving (Eq, Ord, Show)

data BaseActor = BaseActor
    { _actorId   :: !ActorId
    , _name      :: !T.Text
    , _position  :: !Point
    , _symbol    :: !Char
    , _sgr       :: ![ANSI.SGR]
    , _hitpoints :: !Int
    }
    deriving (Show)

makeClassy ''BaseActor
