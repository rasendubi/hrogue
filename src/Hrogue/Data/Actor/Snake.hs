{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Hrogue.Data.Actor.Snake (Snake(Snake)) where

import           Control.Lens            (use, uses, view, (^.))
import           Control.Lens.TH         (makeClassy)

import           Control.Monad           (forM_)

import           Hrogue.Control.HrogueM

import qualified Algorithm.Search        as S

import qualified Hrogue.Data.Actor       as Actor
import qualified Hrogue.Data.HrogueState as HrogueState
import           Hrogue.Data.Level       (TerrainMap, isWalkable,
                                          terrainMapCell)
import           Hrogue.Data.Point       (Point (Point), directions, pointMinus)

data Snake = Snake
    { _baseActor :: Actor.BaseActor
    }
    deriving (Show)

makeClassy ''Snake

instance Actor.HasBaseActor Snake where baseActor = baseActor

instance Actor.Actor (HrogueM ()) Snake where
  takeTurn = snakeTurn

snakeTurn :: Snake -> HrogueM ()
snakeTurn a = do
  let actorId = a ^. Actor.actorId
  let currentPos = a ^. Actor.position
  terrain <- use HrogueState.terrainMap
  mplayer <- uses (HrogueState.actor playerId) (fmap $ view Actor.position)
  let mnext = mplayer >>= \player -> do
        (_price, path) <- searchPath currentPos player terrain
        return $ head path

  forM_ mnext $ \next ->
    moveActor actorId (next `pointMinus` currentPos)

searchPath :: Point -> Point -> TerrainMap -> Maybe (Int, [Point])
searchPath from to terrainMap = S.aStar (next `S.pruning` isWall) cost estimate (== to) from
  where
    isWall = not . isWalkable . terrainMapCell terrainMap
    next p = fmap (p <>) directions
    cost (Point _x1 _y1) (Point _x2 _y2) = 1 -- all neighboring cells have cost of 1
    estimate (Point x1 y1) = abs (x1 - x2) + abs (y1 - y2)
      where (Point x2 y2) = to
