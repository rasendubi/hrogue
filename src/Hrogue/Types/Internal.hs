{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Hrogue.Types.Internal
  ( HrogueState(..)
  , terrainMap
  , actors
  , nextId
  , message
  , rng

  , HrogueM
  , runHrogueM

  , Actor(..)
  , ActorM
  , runActorM

  , AnyActor(..)
  , Action(..)
  , HasAction(..)
  ) where

import           Polysemy (Embed, Sem, embed, runM)
import qualified Polysemy.State as State

import           Control.Lens (lens, (&), (.~), (^.))
import           Control.Lens.TH (makeClassy, makeLenses)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified System.Random.Mersenne.Pure64 as MT

import           Hrogue.Data.Level (TerrainMap)
import qualified Hrogue.Types.Internal.BaseActor as BaseActor

-- newtype HrogueM a = HrogueM { unHrogueM :: StateT HrogueState IO a }
--   deriving (Functor, Applicative, Monad, MonadState HrogueState, MonadIO)

type HrogueM a = Sem '[State.State HrogueState, Embed IO] a

runHrogueM :: HrogueM a -> HrogueState -> IO (HrogueState, a)
runHrogueM m s = runM . State.runState s $ m

data Action = Action
  { _run  :: !(AnyActor -> HrogueM ())
  , _cost :: !Int
  }

type ActorM actor a = Sem '[State.State actor, State.State HrogueState, Embed IO] a

runActorM :: actor -> HrogueState -> ActorM actor a -> IO (actor, a)
runActorM actor state = runM . State.evalState state . State.runState actor

class Actor actor where
  takeTurn :: ActorM actor Action

data AnyActor = forall state . (BaseActor.HasBaseActor state, Actor state) =>
  AnyActor state

instance BaseActor.HasBaseActor AnyActor where
  baseActor = lens
    (\(AnyActor a) -> a ^. BaseActor.baseActor)
    (\(AnyActor a) x -> AnyActor (a & BaseActor.baseActor .~ x))

instance Actor AnyActor where
  takeTurn = do
    AnyActor a <- State.get
    state <- State.get @HrogueState
    (a', r) <- embed $ runActorM a state takeTurn
    State.put $ AnyActor a'
    return r

data HrogueState = HrogueState
    { _terrainMap :: !TerrainMap
    , _actors     :: !(Map.Map BaseActor.ActorId AnyActor)
    , _nextId     :: !BaseActor.ActorId
    , _message    :: !(Maybe T.Text)
    , _rng        :: !MT.PureMT
    }

-- HrogueState does not need to be polymorphic
makeLenses ''HrogueState
makeClassy ''Action
