{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Hrogue.Types.Internal
  ( HrogueState(..)
  , terrainMap
  , actors
  , message

  , HrogueM
  , HrogueE
  , runHrogueM

  , Actor(..)
  , ActorM
  , withActor

  , AnyActor(..)
  , Action(..)
  , HasAction(..)
  , mkAction
  ) where

import           Polysemy (Embed, Members, Sem, raise, runM)
import           Polysemy.RandomFu (RandomFu)
import           Polysemy.RandomFu.State (PureMT, runRandomWithState)
import qualified Polysemy.State as State

import           Control.Lens (lens, (&), (.~), (^.))
import           Control.Lens.TH (makeClassy, makeLenses)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import           Hrogue.Data.Level (TerrainMap)
import qualified Hrogue.Types.Internal.BaseActor as BaseActor

type HrogueE =
  [ State.State HrogueState
  , RandomFu
  , State.State PureMT
  , Embed IO
  ]
type ActorE actor =
  [ State.State actor
  , State.State HrogueState
  , RandomFu
  , State.State PureMT
  , Embed IO
  ]

-- | HrogueM is a concrete Sem monad used to run Hrogue game.
--
-- It is here, so we are able to embed it into Action. Otherwise, you
-- should never use it directly, but specify all necessary constraints
-- with 'Members'.
type HrogueM a = Sem HrogueE a

runHrogueM :: PureMT -> HrogueState -> HrogueM a -> IO (PureMT, (HrogueState, a))
runHrogueM g s = runM . State.runState g . runRandomWithState . State.runState s

data Action = Action
  { _run  :: !(AnyActor -> HrogueM ())
  , _cost :: !Int
  }

mkAction :: Int -> (AnyActor -> HrogueM ()) -> Action
mkAction cost' run' = Action run' cost'

-- | HrogueM is a concrete Sem monad used to execute Actor AIs.
--
-- It is here, so we are able to define Actor typeclass without
-- MultiParamTypeClasses. Otherwise, you should never use it directly,
-- but specify all necessary constraints with 'Members'.
type ActorM actor a = Sem (ActorE actor) a

withActor :: (Members HrogueE r) => actor -> Sem (State.State actor ': r) a -> Sem r (actor, a)
withActor actor = State.runState actor

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
    (a', r) <- raise $ withActor a takeTurn
    State.put $ AnyActor a'
    return r

data HrogueState = HrogueState
    { _terrainMap :: !TerrainMap
    , _actors     :: !(Map.Map BaseActor.ActorId AnyActor)
    , _message    :: !(Maybe T.Text)
    }

-- HrogueState does not need to be polymorphic
makeLenses ''HrogueState
makeClassy ''Action
