{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Main monad of the game
module Hrogue.Control.HrogueM
  ( HrogueM
  , HrogueState(..)
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

import qualified System.Random.Mersenne.Pure64 as MT

import           Hrogue.Data.Actor             (Actor, ActorId (..))
import qualified Hrogue.Data.Actor             as Actor
import           Hrogue.Data.Level             (TerrainMap, isWalkable,
                                                terrainMapCell)
import           Hrogue.Data.Point             (Point)

import           Hrogue.Terminal               as Terminal

type HrogueM = StateT HrogueState IO

data HrogueState = HrogueState
    { hrogueStateTerrainMap :: !TerrainMap
    , hrogueStateActors     :: !(Map.Map ActorId AnyActor)
    , hrogueStateNextId     :: !ActorId
    , hrogueStateRng        :: !MT.PureMT
    , hrogueStateMessage    :: !(Maybe T.Text)
    }
    deriving (Show)

class ActorType actorState where
  actorName :: Actor actorState -> T.Text
  actorTakeTurn :: Actor actorState -> HrogueM ()

data AnyActor = forall t . (ActorType t, Show t) => AnyActor (Actor t)

deriving instance Show AnyActor

playerId :: ActorId
playerId = ActorId 0

setMessage :: T.Text -> HrogueM ()
setMessage m = modify' $ \state -> state{ hrogueStateMessage = Just m }

getActor :: ActorId -> HrogueM (Maybe AnyActor)
getActor i = (Map.!? i) <$> gets hrogueStateActors

getActorUnsafe :: ActorId -> HrogueM AnyActor
getActorUnsafe i = (Map.! i) <$> gets hrogueStateActors

displayActor :: AnyActor -> IO ()
displayActor (AnyActor a) = Terminal.putSymbol (Actor.actorPosition a) (Actor.actorSgr a) (Actor.actorSymbol a)

moveActor :: ActorId -> Point -> HrogueM ()
moveActor actorId pdiff = do
  terrain <- gets hrogueStateTerrainMap
  (AnyActor actor) <- getActorUnsafe actorId
  let prev = Actor.actorPosition actor
  let next = prev <> pdiff
  let cell = terrainMapCell terrain next

  manotherActor <- actorAtPoint next

  case manotherActor of
    Just (bActorId, AnyActor bActor) -> do
      let nextHitpoints = Actor.actorHitpoints bActor - 10
      if nextHitpoints <= 0
        then do
          deleteActor bActorId
          setMessage $ actorName bActor <> T.pack " is killed"
        else do
          modifyActor bActorId $ const (AnyActor bActor{ Actor.actorHitpoints = nextHitpoints })
          when (actorId == playerId) $
            setMessage $ T.pack "You hit " <> actorName bActor
          when (bActorId == playerId) $
            setMessage $ actorName actor <> T.pack " hits you"
    Nothing ->
      when (isWalkable cell) $
        modifyActor actorId $ \(AnyActor a) -> AnyActor a{ Actor.actorPosition = next }

modifyActor :: ActorId -> (AnyActor -> AnyActor) -> HrogueM ()
modifyActor actorId f =
  modify' $ \state -> state{ hrogueStateActors = Map.adjust f actorId (hrogueStateActors state) }

deleteActor :: ActorId -> HrogueM ()
deleteActor actorId =
  modify' $ \state -> state{ hrogueStateActors = Map.delete actorId (hrogueStateActors state) }

actorAtPoint :: Point -> HrogueM (Maybe (ActorId, AnyActor))
actorAtPoint target = do
  actors <- gets hrogueStateActors
  return $ find (\(_, AnyActor Actor.Actor{ Actor.actorPosition = p }) -> p == target) . Map.toList $ actors
