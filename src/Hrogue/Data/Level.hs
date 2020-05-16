module Hrogue.Data.Level
  ( TerrainMap
  , terrainMapCell
  , terrainMapStartPosition
  , TerrainCell(..)
  , parseMap
  , isWalkable
  , isVisible
  , renderTerrain
  , terrainMapSize
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Hrogue.Data.Point (Point (Point))

data TerrainCell
    = Floor
    | Corridor
    | Wall
    deriving (Eq, Show)

data TerrainMap = TerrainMap
    { unTerrainMap            :: !(Vector (Vector TerrainCell))
    , terrainMapStartPosition :: !Point
    }
    deriving (Show)

terrainMapCell :: TerrainMap -> Point -> TerrainCell
terrainMapCell m (Point x y) = (V.! x) . (V.! y) . unTerrainMap $ m

terrainMapSize :: TerrainMap -> (Int, Int)
terrainMapSize TerrainMap{ unTerrainMap = t } = (V.length (t V.! 0), V.length t)

parseMap :: Text -> TerrainMap
parseMap t = TerrainMap terrain (Point startX startY)
  where
    headerLine:mapLines = T.lines t
    [_sizeX, _sizeY, startX, startY] = map (read . T.unpack) . T.words $ headerLine
    terrain = V.fromList . map (V.fromList . map charToTerrainCell . T.unpack) $ mapLines

isWalkable :: TerrainCell -> Bool
isWalkable Floor    = True
isWalkable Corridor = True
isWalkable _        = False

charToTerrainCell :: Char -> TerrainCell
charToTerrainCell '.' = Floor
charToTerrainCell '<' = Floor
charToTerrainCell '#' = Corridor
charToTerrainCell ' ' = Wall
charToTerrainCell _   = undefined

terrainCellToChar :: TerrainCell -> Char
terrainCellToChar Floor    = '.'
terrainCellToChar Corridor = '#'
terrainCellToChar Wall     = ' '

renderTerrain :: TerrainMap -> V.Vector (V.Vector Char)
renderTerrain TerrainMap{ unTerrainMap = m } =
  V.map (V.map terrainCellToChar) m

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
