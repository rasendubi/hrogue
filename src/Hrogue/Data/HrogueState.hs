module Hrogue.Data.HrogueState
  ( HrogueState(..)
  ) where

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T

import qualified System.Random.Mersenne.Pure64 as MT

import           Hrogue.Data.Level             (TerrainMap)
import           Hrogue.Data.Actor             (ActorId (..))

data HrogueState actor = HrogueState
    { terrainMap :: !TerrainMap
    , actors     :: !(Map.Map ActorId actor)
    , nextId     :: !ActorId
    , rng        :: !MT.PureMT
    , message    :: !(Maybe T.Text)
    }
    deriving (Show)
