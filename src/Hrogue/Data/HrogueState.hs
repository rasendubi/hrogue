{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Hrogue.Data.HrogueState
  ( HrogueState(..)
  , terrainMap
  , actors
  , nextId
  , rng
  , message
  , actor
  , actorAtPoint
  ) where

import           Control.Lens                  (Lens', at, (^.))
import           Control.Lens.TH               (makeLenses)
import           Data.Foldable                 (find)

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T

import qualified System.Random.Mersenne.Pure64 as MT

import           Hrogue.Data.Actor             (ActorId (..), HasBaseActor)
import qualified Hrogue.Data.Actor             as Actor
import           Hrogue.Data.Level             (TerrainMap)
import           Hrogue.Data.Point             (Point)

data HrogueState actor = HrogueState
    { _terrainMap :: !TerrainMap
    , _actors     :: !(Map.Map ActorId actor)
    , _nextId     :: !ActorId
    , _rng        :: !MT.PureMT
    , _message    :: !(Maybe T.Text)
    }

makeLenses ''HrogueState

actor :: ActorId -> Lens' (HrogueState actor) (Maybe actor)
actor idx = actors . at idx

actorAtPoint :: HasBaseActor actor => Point -> HrogueState actor -> Maybe actor
actorAtPoint p state = find (\a -> a ^. Actor.position == p) (state ^. actors)
