module Hrogue.Data.Level (TerrainMap, TerrainCell(..), parseMap, terrainMapToString) where

import           Data.Text   (Text)
import qualified Data.Text   as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

data TerrainCell = Floor
    | Corridor
    | Wall
    deriving (Eq, Show)

newtype TerrainMap = TerrainMap { unTerrainMap :: Vector (Vector TerrainCell) }
  deriving (Show)

parseMap :: Text -> TerrainMap
parseMap = TerrainMap . V.fromList . map (V.fromList . map charToTerrainCell . T.unpack) . T.lines

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
