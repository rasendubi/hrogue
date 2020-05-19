{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Hrogue.Types.Internal
  ( HrogueState(..)
  , HasHrogueState(..)
  , HrogueM
  , runHrogueM
  , Actor(..)
  , AnyActor(..)
  , Action(..)
  , HasAction(..)
  ) where

import           Control.Lens (lens, (&), (.~), (^.))
import           Control.Lens.TH (makeClassy)

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State.Strict
    (MonadState, StateT (..), get, put, runStateT)
import           Control.Monad.Trans (lift)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified System.Random.Mersenne.Pure64 as MT

import           Hrogue.Data.Level (TerrainMap)
import qualified Hrogue.Types.Internal.BaseActor as BaseActor

newtype HrogueM a = HrogueM { unHrogueM :: StateT HrogueState IO a }
  deriving (Functor, Applicative, Monad, MonadState HrogueState, MonadIO)

runHrogueM :: HrogueM a -> HrogueState -> IO (a, HrogueState)
runHrogueM (HrogueM m) s = runStateT m s

data Action = Action
  { _run  :: !(AnyActor -> HrogueM ())
  , _cost :: !Int
  }

class Actor actor where
  takeTurn :: StateT actor HrogueM Action

data AnyActor = forall state . (BaseActor.HasBaseActor state, Actor state) =>
  AnyActor state

instance BaseActor.HasBaseActor AnyActor where
  baseActor = lens
    (\(AnyActor a) -> a ^. BaseActor.baseActor)
    (\(AnyActor a) x -> AnyActor (a & BaseActor.baseActor .~ x))

instance Actor AnyActor where
  takeTurn = do
    AnyActor a <- get
    (r, a') <- lift $ runStateT takeTurn a
    put $ AnyActor a'
    return r

data HrogueState = HrogueState
    { _terrainMap :: !TerrainMap
    , _actors     :: !(Map.Map BaseActor.ActorId AnyActor)
    , _nextId     :: !BaseActor.ActorId
    , _message    :: !(Maybe T.Text)
    , _rng        :: !MT.PureMT
    }

makeClassy ''HrogueState
makeClassy ''Action
