{-# LANGUAGE ScopedTypeVariables #-}
module Hrogue.LevelGen
  ( generateLevel
  ) where

import qualified Control.Monad.Random.Strict as Random
import qualified Control.Monad.ST as ST
import           Control.Monad (replicateM_, forM, forM_, when)
import           Control.Monad.Trans (lift)

import           Data.STRef (newSTRef, readSTRef, modifySTRef')

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import           Hrogue.Data.Point (Point (Point))
import qualified Hrogue.Data.Level as Level

generateLevel :: Random.RandomGen r => (Int, Int) -> r -> (Level.TerrainMap, r)
generateLevel sizes r = (m, r')
  where
    ((v, (x, y)), r') = ST.runST $ Random.runRandT (generateLevel' sizes) r
    m = Level.TerrainMap v (Point x y)

generateLevel'
  :: forall s g. Random.RandomGen g
  => (Int, Int)
  -> Random.RandT g (ST.ST s) (V.Vector (V.Vector Level.TerrainCell), (Int, Int))
generateLevel' (sizeX, sizeY) = do
  res <- V.replicateM sizeY $ VM.replicate sizeX Level.Wall

  rooms' <- lift $ newSTRef []
  corridors' <- lift $ newSTRef []

  let
    pushRoom r = modifySTRef' rooms' (r:)
    pushCorridor r = modifySTRef' corridors' (r:)

    set :: Level.TerrainCell -> (Int, Int) -> ST.ST s ()
    set c (x, y) = VM.write (res V.! y) x c
    get :: (Int, Int) -> ST.ST s Level.TerrainCell
    get   (x, y) = VM.read (res V.! y) x

    fill r c = forM_ (rectCells r) (set c)

    scan r = do
      let cells = rectCells r
      if all (\(x, y) -> x >= 0 && x < sizeX && y >= 0 && y < sizeY) cells
        then all (== Level.Wall) <$> forM cells get
        else return False

    placeRandomRoom = do
      y <- Random.getRandomR (sizeY `div` 4, sizeY `div` 2)
      x <- Random.getRandomR (sizeX `div` 4, sizeX `div` 2)
      roomSizeX <- Random.getRandomR (3, 5)
      roomSizeY <- Random.getRandomR (3, 5)
      let room = ((x, y), (x + roomSizeX, y + roomSizeY))
      lift $ pushRoom room
      lift $ fill room Level.Floor

    tryPlaceFeature = do
      tryFeature <- Random.fromList
        [ (tryPlaceCorridor, 3)
        , (tryPlaceRoom, 2)
        ]
      tryFeature

    tryPlaceCorridor = do
      rooms <- lift $ readSTRef rooms'
      corridors <- lift $ readSTRef corridors'
      room <- Random.uniform $ rooms ++ corridors
      wall <- randomRoomWall room

      corridorLength <- Random.getRandomR (1, 5)
      let
        scanRect = transposeRect wall ((0, -1), (corridorLength + 1, 1)) -- looks to right
        fillRect = transposeRect wall ((1, 0),  (corridorLength, 0))

      free <- lift $ scan scanRect
      when free $ lift $ do
        pushCorridor fillRect
        fill fillRect Level.Corridor
        -- Set entry to the same cell as room it branches off. So the
        -- entry cell is floor if branching from room, or corridor if
        -- branching from corridor.
        fillingCell <- get (fst room)
        set fillingCell (snd wall)


    tryPlaceRoom = do
      rooms <- lift $ readSTRef rooms'
      corridors <- lift $ readSTRef corridors'
      room <- Random.uniform $ rooms ++ corridors
      wall <- randomRoomWall room

      sx <- Random.getRandomR (2, 5)
      sy <- Random.getRandomR (2, 5)
      offset <- Random.getRandomR (0, sy)

      let
        scanRect = transposeRect wall ((0, -offset - 1), (1 + sx + 1, sy - offset + 1))
        fillRect = transposeRect wall ((1, -offset),     (1 + sx,     sy - offset))

      free <- lift $ scan scanRect
      when free $ lift $ do
        pushRoom fillRect
        fill fillRect Level.Floor
        -- entry door
        set Level.Floor (snd wall)


    randomRoomWall ((x1, y1), (x2, y2)) = do
      dir :: Int <- Random.getRandomR (0, 3)
      let wall = case dir of
            0 -> ((x1, y1 - 1), (x2, y1 - 1)) -- up
            1 -> ((x1, y2 + 1), (x2, y2 + 1)) -- down
            2 -> ((x1 - 1, y1), (x1 - 1, y2)) -- left
            3 -> ((x2 + 1, y1), (x2 + 1, y2)) -- right
            _ -> error "Dir is in range 0..3"
      wallCell <- Random.uniform (rectCells wall)
      return (dir, wallCell)

    selectStartPosition = do
      rooms <- lift $ readSTRef rooms'
      room <- Random.uniform rooms
      Random.uniform (rectCells room)


  placeRandomRoom
  replicateM_ 300 tryPlaceFeature

  res' <- V.mapM V.freeze res
  startPosition <- selectStartPosition

  return (res', startPosition)


normalizeRect :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
normalizeRect r@((x1, y1), (x2, y2))
  | x2 < x1 = normalizeRect ((x2, y1), (x1, y2))
  | y2 < y1 = normalizeRect ((x1, y2), (x2, y1))
  | otherwise = r

rectCells :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
rectCells ((x1, y1), (x2, y2)) = do
  y <- [y1 .. y2]
  x <- [x1 .. x2]
  return (x, y)

transposeRect :: (Int, (Int, Int)) -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
transposeRect (dir, (x, y)) ((x1, y1), (x2, y2)) = normalizeRect $
  case dir of
    0 -> ((x + y1, y - x2), (x + y2, y - x1)) -- up (rotate 90 counter clockwise)
    1 -> ((x + y1, y + x2), (x + y2, y + x1)) -- down (rotate 90 clockwise)
    2 -> ((x - x1, y + y1), (x - x2, y + y2)) -- left (swap x)
    3 -> ((x + x1, y + y1), (x + x2, y + y2)) -- right
    _ -> error "dir should be in range 0..3"
