module Hrogue.Data.Level (TerrainMap, terrainMapStartPosition, TerrainCell(..), parseMap, terrainMapToString) where

import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Vector     (Vector)
import qualified Data.Vector     as V

import           Hrogue.Terminal (Point (Point))

data TerrainCell = Floor
    | Corridor
    | Wall
    deriving (Eq, Show)

data TerrainMap = TerrainMap
    { unTerrainMap            :: !(Vector (Vector TerrainCell))
    , terrainMapSize          :: !(Int, Int)
    , terrainMapStartPosition :: !Point
    }
    deriving (Show)

parseMap :: Text -> TerrainMap
parseMap t =
  let
    headerLine:mapLines = T.lines t
    [sizeX, sizeY, startX, startY] = map (read . T.unpack) . T.words $ headerLine
    terrain = V.fromList . map (V.fromList . map charToTerrainCell . T.unpack) $ mapLines
  in TerrainMap terrain (sizeX, sizeY) (Point startX startY)

charToTerrainCell :: Char -> TerrainCell
charToTerrainCell '.' = Floor
charToTerrainCell '<' = Floor
charToTerrainCell '#' = Corridor
charToTerrainCell ' ' = Wall

terrainCellToChar Floor    = '.'
terrainCellToChar Corridor = '#'
terrainCellToChar Wall     = ' '

terrainMapToString =
  T.unlines . V.toList . V.map (T.pack . V.toList . V.map terrainCellToChar) . unTerrainMap
