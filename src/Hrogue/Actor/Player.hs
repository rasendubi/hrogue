{-# LANGUAGE TemplateHaskell #-}
module Hrogue.Actor.Player
  ( Player
  , mkPlayer
  ) where

import           Control.Lens.TH (makeClassy)

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Strict (StateT, lift)

import qualified Data.Text as T

import           System.Exit (exitSuccess)
import           System.IO (hPrint, stderr)

import           Hrogue.Control.HrogueM

import           Hrogue.Terminal (getKey)

import           Hrogue.Action.MoveAttack (moveAttack)
import           Hrogue.Action.Wait (wait)
import qualified Hrogue.Types.Action as Action
import qualified Hrogue.Types.Actor as Actor

import           Hrogue.Data.Point (Point, down, left, right, up)
import qualified Hrogue.Data.Symbol as Symbol

import           Hrogue.Redraw (redraw)

data Player = Player
    { _baseActor :: !Actor.BaseActor
    }
    deriving (Show)

mkPlayer :: ActorId -> Point -> Player
mkPlayer actorId position = Player
  { _baseActor =
      Actor.BaseActor
        { Actor._actorId = actorId
        , Actor._name = T.pack "Player"
        , Actor._position = position
        , Actor._symbol = Symbol.withForeground (Symbol.rgb 2 4 5) $ Symbol.symbol '@'
        , Actor._health = 100
        , Actor._energy = 0
        , Actor._speed = 25
        }
  }

makeClassy ''Player

instance Actor.HasBaseActor Player where
  baseActor = baseActor

instance Actor.Actor Player where
  takeTurn = playerTurn

playerTurn :: StateT Player HrogueM Action.Action
playerTurn = do
  lift redraw
  k <- liftIO getKey
  liftIO $ logKey k
  liftIO $ when (k == "q") exitSuccess
  return $ processKey k

logKey :: String -> IO ()
logKey = hPrint stderr

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
