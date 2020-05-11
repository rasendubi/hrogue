{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Hrogue.Data.HrogueState
  ( HrogueState(..)
  , terrainMap
  , actors
  , nextId
  , rng
  , message
  , ActorWithState(ActorWithState)
  , actorState
  , actor
  , actorAtPoint
  ) where

import           Control.Lens                  (Lens', at, (^.))
import           Control.Lens.TH               (makeLenses)
import           Data.Foldable                 (find)

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T

import qualified System.Random.Mersenne.Pure64 as MT

import           Hrogue.Data.Actor             (Actor, ActorId (..))
import qualified Hrogue.Data.Actor             as Actor
import           Hrogue.Data.Level             (TerrainMap)
import           Hrogue.Data.Point             (Point)

data ActorWithState actorState = ActorWithState
    { _actorWithStateActor :: !Actor
    , _actorState          :: !actorState
    }

data HrogueState actorState = HrogueState
    { _terrainMap :: !TerrainMap
    , _actors     :: !(Map.Map ActorId (ActorWithState actorState))
    , _nextId     :: !ActorId
    , _rng        :: !MT.PureMT
    , _message    :: !(Maybe T.Text)
    }

makeLenses ''ActorWithState
makeLenses ''HrogueState

instance Actor.HasActor (ActorWithState state) where actor = actorWithStateActor

actor :: ActorId -> Lens' (HrogueState state) (Maybe (ActorWithState state))
actor idx = actors . at idx

actorAtPoint :: Point -> HrogueState actorState -> Maybe (ActorWithState actorState)
actorAtPoint p state = find (\a -> a ^. Actor.position == p) (state ^. actors)
