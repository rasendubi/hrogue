{-# LANGUAGE TemplateHaskell #-}
module Hrogue.Actor.Player (Player(Player)) where

import           Control.Lens ((^.))
import           Control.Lens.TH (makeClassy)

import           Control.Monad.IO.Class (liftIO)

import           Hrogue.Control.HrogueM
import           Hrogue.Data.Point (down, left, right, up)

import           Hrogue.Terminal (getKey)

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

playerTurn :: Player -> HrogueM ()
playerTurn actor = do
  k <- liftIO getKey
  processKey actor k

processKey :: Player -> String -> HrogueM ()
processKey a k =
  let move = moveActor $ a ^. Actor.actorId
  in case k of
    "\ESC[A" -> move up
    "e"      -> move up
    "\ESC[B" -> move down
    "n"      -> move down
    "\ESC[C" -> move right
    "o"      -> move right
    "\ESC[D" -> move left
    "y"      -> move left
    "j"      -> move (up   <> left)
    "f"      -> move (up   <> right)
    "v"      -> move (down <> left)
    "k"      -> move (down <> right)
    _        -> return ()
