module Hrogue.Data.Actor.Snake (Snake(Snake)) where

import qualified Data.Text                  as T

import           Hrogue.Control.HrogueM

import           Control.Monad              (forM_)
import           Control.Monad.State.Strict (gets)

import qualified Algorithm.Search           as S

import           Hrogue.Data.Actor          (Actor (Actor))
import qualified Hrogue.Data.Actor          as Actor
import           Hrogue.Data.Level          (TerrainMap, isWalkable,
                                             terrainMapCell)
import           Hrogue.Data.Point          (Point (Point), directions,
                                             pointMinus)

data Snake = Snake
    deriving (Show)

instance ActorType Snake where
  actorName _ = T.pack "Snake"
  actorTakeTurn = snakeTurn

snakeTurn :: Actor Snake -> HrogueM ()
snakeTurn Actor{ Actor.actorId = actorId, Actor.actorPosition = currentPos } = do
  terrain <- gets hrogueStateTerrainMap
  mplayer <- fmap (\(AnyActor actor) -> Actor.actorPosition actor) <$> getActor playerId
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
