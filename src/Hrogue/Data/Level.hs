module Hrogue.Data.Level
  ( TerrainMap
  , terrainMapCell
  , terrainMapStartPosition
  , TerrainCell(..)
  , parseMap
  , terrainMapToString
  , isWalkable
  ) where

import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Vector       (Vector)
import qualified Data.Vector       as V

import           Hrogue.Data.Point (Point (Point))

data TerrainCell
    = Floor
    | Corridor
    | Wall
    deriving (Eq, Show)

data TerrainMap = TerrainMap
    { unTerrainMap            :: !(Vector (Vector TerrainCell))
    , terrainMapSize          :: !(Int, Int)
    , terrainMapStartPosition :: !Point
    }
    deriving (Show)

terrainMapCell :: TerrainMap -> Point -> TerrainCell
terrainMapCell m (Point x y) = (V.! x) . (V.! y) . unTerrainMap $ m

parseMap :: Text -> TerrainMap
parseMap t = TerrainMap terrain (sizeX, sizeY) (Point startX startY)
  where
    headerLine:mapLines = T.lines t
    [sizeX, sizeY, startX, startY] = map (read . T.unpack) . T.words $ headerLine
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

terrainMapToString :: TerrainMap -> Text
terrainMapToString = T.unlines . V.toList . V.map lineToString . unTerrainMap
  where
    lineToString = T.pack . V.toList . V.map terrainCellToChar
