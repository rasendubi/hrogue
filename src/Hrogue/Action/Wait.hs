module Hrogue.Action.Wait
  ( wait
  ) where

import Hrogue.Types.Action (Action, mkAction)

wait :: Action
wait = mkAction 50 $ \_actor -> return ()
