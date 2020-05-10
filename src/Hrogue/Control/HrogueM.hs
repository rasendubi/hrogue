{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Main monad of the game
module Hrogue.Control.HrogueM
  ( HrogueM
  , HrogueState(HrogueState)
  , ActorId(..)
  , ActorType(..)
  , AnyActor(..)
  , playerId
  , setMessage
  , getActor
  , getActorUnsafe
  , displayActor
  , moveActor
  , modifyActor
  , deleteActor
  , actorAtPoint
  ) where

import           Data.List                     (find)

import qualified Data.Map.Strict               as Map

import           Control.Monad                 (when)
import           Control.Monad.State.Strict    (StateT (..), gets, modify')

import qualified Data.Text                     as T

import           Hrogue.Data.Actor             (Actor, ActorId (..))
import qualified Hrogue.Data.Actor             as Actor
import           Hrogue.Data.Level             (isWalkable, terrainMapCell)
import           Hrogue.Data.Point             (Point)

import           Hrogue.Terminal               as Terminal
import Hrogue.Data.HrogueState (HrogueState)
import qualified Hrogue.Data.HrogueState as HrogueState

type HrogueM = StateT (HrogueState AnyActor) IO

class ActorType actorState where
  actorName :: Actor actorState -> T.Text
  actorTakeTurn :: Actor actorState -> HrogueM ()

data AnyActor = forall t . (ActorType t, Show t) => AnyActor (Actor t)

deriving instance Show AnyActor

playerId :: ActorId
playerId = ActorId 0

setMessage :: T.Text -> HrogueM ()
setMessage m = modify' $ \state -> state{ HrogueState.message = Just m }

getActor :: ActorId -> HrogueM (Maybe AnyActor)
getActor i = (Map.!? i) <$> gets HrogueState.actors

getActorUnsafe :: ActorId -> HrogueM AnyActor
getActorUnsafe i = (Map.! i) <$> gets HrogueState.actors

displayActor :: AnyActor -> IO ()
displayActor (AnyActor a) = Terminal.putSymbol (Actor.position a) (Actor.sgr a) (Actor.symbol a)

moveActor :: ActorId -> Point -> HrogueM ()
moveActor actorId pdiff = do
  terrain <- gets HrogueState.terrainMap
  (AnyActor actor) <- getActorUnsafe actorId
  let prev = Actor.position actor
  let next = prev <> pdiff
  let cell = terrainMapCell terrain next

  manotherActor <- actorAtPoint next

  case manotherActor of
    Just (bActorId, AnyActor bActor) -> do
      let nextHitpoints = Actor.hitpoints bActor - 10
      if nextHitpoints <= 0
        then do
          deleteActor bActorId
          setMessage $ actorName bActor <> T.pack " is killed"
        else do
          modifyActor bActorId $ const (AnyActor bActor{ Actor.hitpoints = nextHitpoints })
          when (actorId == playerId) $
            setMessage $ T.pack "You hit " <> actorName bActor
          when (bActorId == playerId) $
            setMessage $ actorName actor <> T.pack " hits you"
    Nothing ->
      when (isWalkable cell) $
        modifyActor actorId $ \(AnyActor a) -> AnyActor a{ Actor.position = next }

modifyActor :: ActorId -> (AnyActor -> AnyActor) -> HrogueM ()
modifyActor actorId f =
  modify' $ \state -> state{ HrogueState.actors = Map.adjust f actorId (HrogueState.actors state) }

deleteActor :: ActorId -> HrogueM ()
deleteActor actorId =
  modify' $ \state -> state{ HrogueState.actors = Map.delete actorId (HrogueState.actors state) }

actorAtPoint :: Point -> HrogueM (Maybe (ActorId, AnyActor))
actorAtPoint target = do
  actors <- gets HrogueState.actors
  return $ find (\(_, AnyActor Actor.Actor{ Actor.position = p }) -> p == target) . Map.toList $ actors
