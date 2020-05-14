{-# LANGUAGE TemplateHaskell #-}
module Hrogue.Types.Internal.BaseActor
  ( ActorId(..)
  , BaseActor(..)
  , HasBaseActor(..)
  ) where

import           Control.Lens.TH (makeClassy)

import qualified Data.Text as T

import qualified System.Console.ANSI as ANSI

import           Hrogue.Data.Point (Point)

newtype ActorId = ActorId { unActorId :: Int }
  deriving (Eq, Ord, Show)

data BaseActor = BaseActor
    { _actorId  :: !ActorId
    , _name     :: !T.Text
    , _position :: !Point
    , _symbol   :: !Char
    , _sgr      :: ![ANSI.SGR]
    , _health   :: !Int
    , _energy   :: !Int
    , _speed    :: !Int
    }
    deriving (Show)

makeClassy ''BaseActor
