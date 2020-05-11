{-# LANGUAGE ExistentialQuantification #-}

-- | Main monad of the game
module Hrogue.Control.HrogueM
  ( HrogueM
  , HrogueState(HrogueState)
  , ActorId(..)
  , ActorType(..)
  , AnyActorState(..)
  , ActorWithAnyState
  , ActorWithState(..)
  , playerId
  , setMessage
  , displayActor
  , moveActor
  , modifyActor
  , deleteActor
  , actorAtPoint
  ) where

import           Control.Lens               (at, use, (%=), (&), (.=), (.~),
                                             (^.))

import           Control.Monad              (when)
import           Control.Monad.State.Strict (StateT (..), gets)

import qualified Data.Text                  as T

import           Hrogue.Data.Actor          (ActorId (..))
import qualified Hrogue.Data.Actor          as Actor
import           Hrogue.Data.Level          (isWalkable, terrainMapCell)
import           Hrogue.Data.Point          (Point)

import           Hrogue.Data.HrogueState    (ActorWithState (ActorWithState),
                                             HrogueState)
import qualified Hrogue.Data.HrogueState    as HrogueState
import           Hrogue.Terminal            as Terminal

type HrogueM = StateT (HrogueState AnyActorState) IO

type ActorWithAnyState = ActorWithState AnyActorState

class ActorType actorState where
  actorName :: ActorWithState actorState -> T.Text
  actorTakeTurn :: ActorWithState actorState -> HrogueM ()

instance ActorType AnyActorState where
  actorName (ActorWithState a (AnyActorState x)) = actorName (ActorWithState a x)
  actorTakeTurn (ActorWithState a (AnyActorState x)) = actorTakeTurn (ActorWithState a x)

data AnyActorState = forall state . ActorType state => AnyActorState state

playerId :: ActorId
playerId = ActorId 0

setMessage :: T.Text -> HrogueM ()
setMessage m = HrogueState.message .= Just m

displayActor :: ActorWithAnyState -> IO ()
displayActor a =
  Terminal.putSymbol (a ^. Actor.position) (a ^. Actor.sgr) (a ^. Actor.symbol)

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
      let nextHitpoints = bActor ^. Actor.hitpoints - 10
      if nextHitpoints <= 0
        then do
          HrogueState.actor bActorId .= Nothing
          setMessage $ actorName bActor <> T.pack " is killed"
        else do
          HrogueState.actor bActorId .= Just (bActor & Actor.hitpoints .~ nextHitpoints)
          when (actorId == playerId) $
            setMessage $ T.pack "You hit " <> actorName bActor
          when (bActorId == playerId) $
            setMessage $ actorName actor <> T.pack " hits you"
    Nothing ->
      when (isWalkable cell) $
        modifyActor actorId $ \a -> a & Actor.position .~ next

modifyActor :: ActorId -> (ActorWithAnyState -> ActorWithAnyState) -> HrogueM ()
modifyActor actorId f = HrogueState.actor actorId %= fmap f

deleteActor :: ActorId -> HrogueM ()
deleteActor actorId = HrogueState.actors . at actorId .= Nothing

actorAtPoint :: Point -> HrogueM (Maybe ActorWithAnyState)
actorAtPoint = gets . HrogueState.actorAtPoint
