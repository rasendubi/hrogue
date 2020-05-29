module Hrogue.Types.Actor
  ( module Hrogue.Types.Internal.BaseActor
  , Actor(..)
  , AnyActor(..)
  , ActorM
  , runActorM
  ) where

import           Hrogue.Types.Internal
    (Actor (..), ActorM, AnyActor (..), runActorM)
import           Hrogue.Types.Internal.BaseActor
