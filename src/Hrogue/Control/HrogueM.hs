-- | Main monad of the game
module Hrogue.Control.HrogueM
  ( HrogueM
  , HrogueState(HrogueState)
  , ActorId(..)
  , AnyActor(AnyActor)
  , playerId
  , setMessage
  , moveActor
  , modifyActor
  , deleteActor
  , actorAtPoint
  ) where

import           Control.Lens (at, use, (%=), (&), (.=), (.~), (^.))

import           Control.Monad (when)
import           Control.Monad.State.Strict (gets)

import qualified Data.Text as T

import           Hrogue.Data.Level (isWalkable, terrainMapCell)
import           Hrogue.Data.Point (Point)

import           Hrogue.Types.Actor (ActorId (ActorId), AnyActor)
import qualified Hrogue.Types.Actor as Actor
import           Hrogue.Types.HrogueState (HrogueState)
import qualified Hrogue.Types.HrogueState as HrogueState
import           Hrogue.Types.Internal (HrogueM)

playerId :: ActorId
playerId = ActorId 0

setMessage :: T.Text -> HrogueM ()
setMessage m = HrogueState.message .= Just m

moveActor :: ActorId -> Point -> HrogueM ()
moveActor actorId pdiff = do
  terrain <- use HrogueState.terrainMap
  Just actor <- use $ HrogueState.actor actorId
  let prev = actor ^. Actor.position
  let next = prev <> pdiff
  let cell = terrainMapCell terrain next

  manotherActor <- actorAtPoint next

  case manotherActor of
    Just bActor -> do
      let bActorId = bActor ^. Actor.actorId
      let nextHealth = bActor ^. Actor.health - 10
      if nextHealth <= 0
        then do
          HrogueState.actor bActorId .= Nothing
          setMessage $ bActor ^. Actor.name <> T.pack " is killed"
        else do
          HrogueState.actor bActorId .= Just (bActor & Actor.health .~ nextHealth)
          when (actorId == playerId) $
            setMessage $ T.pack "You hit " <> bActor ^. Actor.name
          when (bActorId == playerId) $
            setMessage $ actor ^. Actor.name <> T.pack " hits you"
    Nothing ->
      when (isWalkable cell) $
        modifyActor actorId $ \a -> a & Actor.position .~ next

modifyActor :: ActorId -> (AnyActor -> AnyActor) -> HrogueM ()
modifyActor actorId f = HrogueState.actor actorId %= fmap f

deleteActor :: ActorId -> HrogueM ()
deleteActor actorId = HrogueState.actors . at actorId .= Nothing

actorAtPoint :: Point -> HrogueM (Maybe AnyActor)
actorAtPoint = gets . HrogueState.actorAtPoint
