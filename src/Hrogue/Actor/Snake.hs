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

import           Hrogue.Data.Level (TerrainMap, isWalkable, terrainMapCell)
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

isVisible :: Point -> Point -> TerrainMap -> Bool
isVisible (Point x1 y1) (Point x2 y2) terrain = all transparent los
  where
    los = bla (x1, y1) (x2, y2)
    transparent (x, y) = isWalkable $ terrainMapCell terrain (Point x y)

bla :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bla p1 p2 = takeWhile (/= p2) $ bla' p1 p2

-- | Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
bla' :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bla' (x0, y0) (x1, y1) = walk (balancedWord p q 0) (x0, y0)
  where
    (dx, dy) = (x1 - x0, y1 - y0)
    xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
    yxStep b (x, y) = (x + signum dx * b, y + signum dy)
    (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                 | otherwise       = (abs dx, abs dy, yxStep)
    walk w xy = xy : walk (tail w) (step (head w) xy)

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
balancedWord p q eps = 1 : balancedWord p q (eps + p - q)