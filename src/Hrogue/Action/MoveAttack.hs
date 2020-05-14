module Hrogue.Action.MoveAttack
  ( moveAttack
  ) where

import           Control.Monad (when)

import           Control.Lens (use, (&), (.=), (.~), (^.), _Just)

import qualified Data.Text as T

import           Hrogue.Data.Level (isWalkable, terrainMapCell)
import           Hrogue.Data.Point (Point)

import           Hrogue.Types.Action (Action, mkAction)
import qualified Hrogue.Types.Actor as Actor
import qualified Hrogue.Types.HrogueState as HrogueState


import           Hrogue.Control.HrogueM

moveAttack :: Point -> Action
moveAttack pdiff = mkAction 100 $ \actor -> do
  terrain <- use HrogueState.terrainMap

  let actorId = actor ^. Actor.actorId
  let prev = actor ^. Actor.position
  let next = prev <> pdiff
  let cell = terrainMapCell terrain next

  manotherActor <- actorAtPoint next

  case manotherActor of
    Nothing ->
      when (isWalkable cell) $
        modifyActor actorId $ \a -> a & Actor.position .~ next

    Just bActor -> do
      let bActorId = bActor ^. Actor.actorId
      let nextHealth = bActor ^. Actor.health - 10
      if nextHealth <= 0
        then do
          HrogueState.actor bActorId .= Nothing

          setMessage $ bActor ^. Actor.name <> T.pack " is killed"
        else do
          HrogueState.actor bActorId . _Just . Actor.health .= nextHealth

          when (actorId == playerId) $
            setMessage $ T.pack "You hit " <> bActor ^. Actor.name
          when (bActorId == playerId) $
            setMessage $ actor ^. Actor.name <> T.pack " hits you"
