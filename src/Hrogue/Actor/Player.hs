{-# LANGUAGE TemplateHaskell #-}
module Hrogue.Actor.Player
  ( Player
  , mkPlayer
  ) where

import           Control.Lens (uses, (<%=), (^.))
import           Control.Lens.TH (makeClassy)

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Strict (StateT, get, lift)

import qualified Data.Text as T

import qualified Data.Vector as V

import           System.Exit (exitSuccess)
import           System.IO (hPrint, stderr)

import           Hrogue.Control.HrogueM

import           Hrogue.Terminal (getKey)

import           Hrogue.Action.MoveAttack (moveAttack)
import           Hrogue.Action.Wait (wait)
import qualified Hrogue.Types.Action as Action
import qualified Hrogue.Types.Actor as Actor
import qualified Hrogue.Types.HrogueState as HrogueState

import           Hrogue.Data.Level (TerrainMap, isVisible, terrainMapSize)
import           Hrogue.Data.Point (Point (Point), down, left, right, up)
import qualified Hrogue.Data.Symbol as Symbol

import           Hrogue.Redraw (redraw)

data Player = Player
    { _baseActor :: !Actor.BaseActor
    , _knownMap  :: !(V.Vector (V.Vector Bool))
    }
    deriving (Show)

mkPlayer :: ActorId -> Point -> (Int, Int) -> Player
mkPlayer actorId position (sizeX, sizeY) = Player
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
  , _knownMap = V.replicate sizeY $ V.replicate sizeX False
  }

makeClassy ''Player

instance Actor.HasBaseActor Player where
  baseActor = baseActor

instance Actor.Actor Player where
  takeTurn = playerTurn

playerTurn :: StateT Player HrogueM Action.Action
playerTurn = do
  p <- get
  v <- lift $ actorVisibilityMap p
  km <- knownMap <%= V.zipWith (V.zipWith (||)) v
  lift $ redraw km v

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

actorVisibilityMap :: Actor.HasBaseActor actor => actor -> HrogueM (V.Vector (V.Vector Bool))
actorVisibilityMap actor = do
  let pos = actor ^. Actor.position
  uses HrogueState.terrainMap (visibilityMap pos)

visibilityMap :: Point -> TerrainMap -> V.Vector (V.Vector Bool)
visibilityMap p terrain = V.generate sizeY $ \y -> V.generate sizeX $ \x -> visible x y
  where
    (sizeX, sizeY) = terrainMapSize terrain
    visible x y = isVisible p (Point x y) terrain
