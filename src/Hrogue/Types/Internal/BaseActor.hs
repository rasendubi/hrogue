{-# LANGUAGE TemplateHaskell #-}
module Hrogue.Types.Internal.BaseActor
  ( ActorId(..)
  , BaseActor(..)
  , HasBaseActor(..)
  ) where

import           Control.Lens.TH (makeClassy)

import qualified Data.Text as T

import qualified Hrogue.Data.Symbol as Symbol
import           Hrogue.Data.Point (Point)

newtype ActorId = ActorId { unActorId :: Int }
  deriving (Eq, Ord, Show)

data BaseActor = BaseActor
    { _actorId  :: !ActorId
    , _name     :: !T.Text
    , _position :: !Point
    , _symbol   :: !Symbol.Symbol
    , _health   :: !Int
    , _energy   :: !Int
    , _speed    :: !Int
    }
    deriving (Show)

makeClassy ''BaseActor
