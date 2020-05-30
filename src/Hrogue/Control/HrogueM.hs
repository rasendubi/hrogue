-- | Main monad of the game
module Hrogue.Control.HrogueM
  ( HrogueE
  , runHrogueM
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

import           Polysemy (Member, Sem)
import           Polysemy.State (State, get, gets, modify')

import           Control.Lens (at, view, (%~), (&), (.~), (^.))

import           Control.Monad (forM_, when)

import qualified Data.Text as T

import           Hrogue.Data.Level (isWalkable, terrainMapCell)
import           Hrogue.Data.Point (Point)

import           Hrogue.Types.Actor (ActorId (ActorId), AnyActor)
import qualified Hrogue.Types.Actor as Actor
import           Hrogue.Types.HrogueState (HrogueState)
import qualified Hrogue.Types.HrogueState as HrogueState
import           Hrogue.Types.Internal (HrogueE, runHrogueM)

playerId :: ActorId
playerId = ActorId 0

setMessage :: (Member (State HrogueState) r) => T.Text -> Sem r ()
setMessage m = modify' @HrogueState $ HrogueState.message .~ Just m

moveActor :: (Member (State HrogueState) r) => ActorId -> Point -> Sem r ()
moveActor actorId pdiff = do
  terrain <- view HrogueState.terrainMap <$> get
  mactor <- view (HrogueState.actor actorId) <$> get
  forM_ mactor $ \actor -> do
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
            modify' @HrogueState $ HrogueState.actor bActorId .~ Nothing
            setMessage $ bActor ^. Actor.name <> T.pack " is killed"
          else do
            modify' @HrogueState $ HrogueState.actor bActorId .~ Just (bActor & Actor.health .~ nextHealth)
            when (actorId == playerId) $
              setMessage $ T.pack "You hit " <> bActor ^. Actor.name
            when (bActorId == playerId) $
              setMessage $ actor ^. Actor.name <> T.pack " hits you"
      Nothing ->
        when (isWalkable cell) $
          modifyActor actorId $ \a -> a & Actor.position .~ next

modifyActor :: (Member (State HrogueState) r) => ActorId -> (AnyActor -> AnyActor) -> Sem r ()
modifyActor actorId f = modify' $ HrogueState.actor actorId %~ fmap f

deleteActor :: (Member (State HrogueState) r) => ActorId -> Sem r ()
deleteActor actorId = modify' $ HrogueState.actors . at actorId .~ Nothing

actorAtPoint :: (Member (State HrogueState) r) => Point -> Sem r (Maybe AnyActor)
actorAtPoint = gets . HrogueState.actorAtPoint
