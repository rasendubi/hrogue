{-# LANGUAGE TemplateHaskell #-}
module Hrogue.Actor.Snake
  ( Snake
  , mkSnake
  ) where

import qualified Algorithm.Search as S

import           Data.Maybe (fromMaybe)

import qualified Data.Text as T

import           Control.Monad (when)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans (lift)

import           Control.Lens (use, uses, view, (.=))
import           Control.Lens.TH (makeClassy)

import qualified System.Console.ANSI as ANSI
import qualified System.Random as Random

import           Hrogue.Data.Level
    (TerrainMap, isVisible, isWalkable, terrainMapCell)
import           Hrogue.Data.Point (Point (Point), directions, pointMinus)

import qualified Hrogue.Types.Action as Action
import qualified Hrogue.Types.Actor as Actor
import qualified Hrogue.Types.HrogueState as HrogueState

import           Hrogue.Action.MoveAttack (moveAttack)
import           Hrogue.Action.Wait (wait)

import           Hrogue.Control.HrogueM

data Snake = Snake
    { _baseActor      :: !Actor.BaseActor
    , _haveSeenPlayer :: !Bool
    }
    deriving (Show)

mkSnake :: ActorId -> Point -> Snake
mkSnake actorId position = Snake
  { _baseActor =
      Actor.BaseActor
        { Actor._actorId = actorId
        , Actor._name = T.pack "Snake"
        , Actor._position = position
        , Actor._symbol = 's'
        , Actor._sgr =
          [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
          , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green
          ]
        , Actor._health = 30
        , Actor._energy = 0
        , Actor._speed = 15
        }
  , _haveSeenPlayer = False
  }

makeClassy ''Snake

instance Actor.HasBaseActor Snake where baseActor = baseActor

instance Actor.Actor Snake where
  takeTurn = snakeTurn

snakeTurn :: StateT Snake HrogueM Action.Action
snakeTurn = do
  currentPos <- use Actor.position
  terrain <- lift $ use HrogueState.terrainMap

  Just player <- lift $ uses (HrogueState.actor playerId) (fmap $ view Actor.position)

  let visible = isVisible currentPos player terrain
  when visible $ haveSeenPlayer .= True

  seenPlayer <- use haveSeenPlayer

  lift $ if visible || seenPlayer
    then
      return $ fromMaybe wait $
        searchPath currentPos player terrain >>= \(_price, path) ->
          return $ moveAttack (head path `pointMinus` currentPos)
    else do
      rng <- use HrogueState.rng
      let (r, rng') = Random.randomR (0, 8) rng
      HrogueState.rng .= rng'
      return $ case r :: Int of
        8 -> wait
        _ -> moveAttack $ directions !! r

searchPath :: Point -> Point -> TerrainMap -> Maybe (Int, [Point])
searchPath from to terrainMap = S.aStar (next `S.pruning` isWall) cost estimate (== to) from
  where
    isWall = not . isWalkable . terrainMapCell terrainMap
    next p = fmap (p <>) directions
    cost (Point _x1 _y1) (Point _x2 _y2) = 1 -- all neighboring cells have cost of 1
    estimate (Point x1 y1) = abs (x1 - x2) + abs (y1 - y2)
      where (Point x2 y2) = to
