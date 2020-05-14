module Hrogue.Types.Action
  ( Action
  , HasAction(..)
  , mkAction
  ) where

import           Hrogue.Types.Internal
    (Action (..), AnyActor, HasAction (..), HrogueM)

mkAction :: Int -> (AnyActor -> HrogueM ()) -> Action
mkAction cost' run' = Action run' cost'
