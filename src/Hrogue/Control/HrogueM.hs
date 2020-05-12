{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

-- | Main monad of the game
module Hrogue.Control.HrogueM
  ( HrogueM
  , HrogueState(HrogueState)
  , ActorId(..)
  , AnyActor(AnyActor)
  , playerId
  , setMessage
  , displayActor
  , moveActor
  , modifyActor
  , deleteActor
  , actorAtPoint
  ) where

import           Control.Lens               (at, lens, use, (%=), (&), (.=),
                                             (.~), (^.))

import           Control.Monad              (when)
import           Control.Monad.State.Strict (StateT (..), gets)

import qualified Data.Text                  as T

import           Hrogue.Data.Actor          (ActorId (..))
import qualified Hrogue.Data.Actor          as Actor
import           Hrogue.Data.Level          (isWalkable, terrainMapCell)
import           Hrogue.Data.Point          (Point)

import           Hrogue.Data.HrogueState    (HrogueState)
import qualified Hrogue.Data.HrogueState    as HrogueState
import           Hrogue.Terminal            as Terminal

type HrogueM = StateT (HrogueState AnyActor) IO

data AnyActor = forall state .
  (Actor.HasBaseActor state, Actor.Actor (HrogueM ()) state) =>
  AnyActor state

instance Actor.HasBaseActor AnyActor where
  baseActor = lens
    (\(AnyActor a) -> a ^. Actor.baseActor)
    (\(AnyActor a) x -> AnyActor (a & Actor.baseActor .~ x))

instance Actor.Actor (HrogueM ()) AnyActor where
  takeTurn (AnyActor a) = Actor.takeTurn a

playerId :: ActorId
playerId = ActorId 0

setMessage :: T.Text -> HrogueM ()
setMessage m = HrogueState.message .= Just m

displayActor :: Actor.HasBaseActor actor => actor -> IO ()
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
          setMessage $ bActor ^. Actor.name <> T.pack " is killed"
        else do
          HrogueState.actor bActorId .= Just (bActor & Actor.hitpoints .~ nextHitpoints)
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
