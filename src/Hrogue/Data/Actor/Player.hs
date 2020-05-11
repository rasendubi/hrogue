module Hrogue.Data.Actor.Player (Player(Player)) where

import           Control.Lens           ((^.))

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text              as T

import           Hrogue.Control.HrogueM
import qualified Hrogue.Data.Actor      as Actor
import           Hrogue.Data.Point      (down, left, right, up)

import           Hrogue.Terminal        (getKey)

data Player = Player
    deriving (Show)

instance ActorType Player where
  actorName _ = T.pack "Player"
  actorTakeTurn = playerTurn

playerTurn :: ActorWithState Player -> HrogueM ()
playerTurn actor = do
  k <- liftIO getKey
  processKey actor k

processKey :: ActorWithState Player -> String -> HrogueM ()
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
