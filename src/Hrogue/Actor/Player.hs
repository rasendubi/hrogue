{-# LANGUAGE TemplateHaskell #-}
module Hrogue.Actor.Player (Player(Player)) where

import           Control.Lens.TH (makeClassy)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Strict (StateT)

import           Hrogue.Control.HrogueM
import           Hrogue.Data.Point (down, left, right, up)

import           Hrogue.Terminal (getKey)

import           Hrogue.Action.MoveAttack (moveAttack)
import           Hrogue.Action.Wait (wait)
import qualified Hrogue.Types.Action as Action
import qualified Hrogue.Types.Actor as Actor

data Player = Player
    { _baseActor :: Actor.BaseActor
    }
    deriving (Show)

makeClassy ''Player

instance Actor.HasBaseActor Player where
  baseActor = baseActor

instance Actor.Actor Player where
  takeTurn = playerTurn

playerTurn :: StateT Player HrogueM Action.Action
playerTurn = processKey <$> liftIO getKey

processKey :: String -> Action.Action
processKey k =
  case k of
    "\ESC[A" -> moveAttack up
    "e"      -> moveAttack up
    "\ESC[B" -> moveAttack down
    "n"      -> moveAttack down
    "\ESC[C" -> moveAttack right
    "o"      -> moveAttack right
    "\ESC[D" -> moveAttack left
    "y"      -> moveAttack left
    "j"      -> moveAttack (up   <> left)
    "f"      -> moveAttack (up   <> right)
    "v"      -> moveAttack (down <> left)
    "k"      -> moveAttack (down <> right)
    _        -> wait
