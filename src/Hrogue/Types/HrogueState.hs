{-# LANGUAGE RankNTypes #-}
module Hrogue.Types.HrogueState
  ( HrogueState(..)
  , HasHrogueState(..)
  , actor
  , actorAtPoint
  , ActorId
  ) where

import           Control.Lens (Lens', at, (^.))

import           Data.Foldable (find)

import           Hrogue.Data.Point (Point)

import           Hrogue.Types.Actor (ActorId, AnyActor)
import qualified Hrogue.Types.Actor as Actor
import           Hrogue.Types.Internal (HasHrogueState (..), HrogueState (..))

actor :: ActorId -> Lens' HrogueState (Maybe AnyActor)
actor idx = actors . at idx

actorAtPoint :: Point -> HrogueState -> Maybe AnyActor
actorAtPoint p state = find (\a -> a ^. Actor.position == p) (state ^. actors)
