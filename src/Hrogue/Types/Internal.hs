{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell  #-}
module Hrogue.Types.Internal
  ( HrogueState(..)
  , HasHrogueState(..)
  , HrogueM
  , Actor(..)
  , AnyActor(..)
  ) where

import           Control.Lens                    ((^.), lens, (&), (.~))
import           Control.Lens.TH                 (makeClassy)

import           Control.Monad.State.Strict      (StateT (..))

import qualified Data.Map.Strict                 as Map
import qualified Data.Text                       as T

import qualified System.Random.Mersenne.Pure64   as MT

import qualified Hrogue.Types.Internal.BaseActor as BaseActor
import           Hrogue.Data.Level               (TerrainMap)

type HrogueM = StateT HrogueState IO

class Actor actor where
  takeTurn :: actor -> HrogueM ()

data AnyActor = forall state .
  (BaseActor.HasBaseActor state, Actor state) =>
  AnyActor state

instance BaseActor.HasBaseActor AnyActor where
  baseActor = lens
    (\(AnyActor a) -> a ^. BaseActor.baseActor)
    (\(AnyActor a) x -> AnyActor (a & BaseActor.baseActor .~ x))

instance Actor AnyActor where
  takeTurn (AnyActor a) = takeTurn a

data HrogueState = HrogueState
    { _terrainMap :: !TerrainMap
    , _actors     :: !(Map.Map BaseActor.ActorId AnyActor)
    , _nextId     :: !BaseActor.ActorId
    , _rng        :: !MT.PureMT
    , _message    :: !(Maybe T.Text)
    }

makeClassy ''HrogueState
