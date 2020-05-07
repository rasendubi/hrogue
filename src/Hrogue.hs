module Hrogue (run) where

import           Control.Monad              (forM_, unless, void, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT (..), get, gets, modify',
                                             put)

import qualified Data.Map.Strict            as Map
import           Data.Text.IO               as T

import qualified System.Console.ANSI        as ANSI
import qualified System.Console.ANSI.Types  as ANSI

import           Hrogue.Data.Actor          (Actor)
import qualified Hrogue.Data.Actor          as Actor
import           Hrogue.Data.Level          (TerrainMap, isWalkable, parseMap,
                                             terrainMapCell,
                                             terrainMapStartPosition,
                                             terrainMapToString)
import           Hrogue.Data.Point          (Point (Point), pointPlus)
import           Hrogue.Terminal            (clearScreen, getKey, goto,
                                             putSymbol, withTerminal)

newtype ActorId = ActorId { unActorId :: Int }
  deriving (Eq, Ord, Show)

data HrogueState = HrogueState
    { hrougeStateTerrainMap :: !TerrainMap
    , hrougeStateActors     :: !(Map.Map ActorId Actor.Actor)
    , hrougeStateNextId     :: !ActorId
    }
    deriving (Show)

type HrogueM = StateT HrogueState IO

playerId = ActorId 0

run :: IO ()
run = withTerminal $ do
  level <- parseMap <$> T.readFile "data/level.txt"
  let actors = Map.fromList
        [ ( playerId
          , Actor.Actor { Actor.actorType = Actor.Player
                        , Actor.actorPosition = terrainMapStartPosition level
                        , Actor.actorSymbol = '@'
                        , Actor.actorSgr = [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
                                           , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan
                                           ]
                        }
          )
        , ( ActorId 1
          , Actor.Actor { Actor.actorType = Actor.Snake
                        , Actor.actorPosition = Point 77 9
                        , Actor.actorSymbol  = 's'
                        , Actor.actorSgr = [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
                                           , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green
                                           ]
                        }
          )
        ]
  let initialState = HrogueState level actors (ActorId 2)
  void $ runStateT game initialState

game :: HrogueM ()
game = do
  redraw
  k <- liftIO getKey
  processKey k
  unless (k == "q") game

redraw :: HrogueM ()
redraw = do
  level <- gets hrougeStateTerrainMap
  actors <- gets hrougeStateActors
  liftIO $ do
    clearScreen

    goto (Point 0 0)
    ANSI.setSGR []
    T.putStr (terrainMapToString level)

    forM_ (Map.elems actors) Actor.displayActor

processKey :: String -> HrogueM ()
processKey k =
  case k of
    "\ESC[A" -> movePlayer up
    "e"      -> movePlayer up
    "\ESC[B" -> movePlayer down
    "n"      -> movePlayer down
    "\ESC[C" -> movePlayer right
    "o"      -> movePlayer right
    "\ESC[D" -> movePlayer left
    "y"      -> movePlayer left
    "j"      -> movePlayer (up   `pointPlus` left)
    "f"      -> movePlayer (up   `pointPlus` right)
    "v"      -> movePlayer (down `pointPlus` left)
    "k"      -> movePlayer (down `pointPlus` right)
    _        -> return ()

movePlayer :: Point -> HrogueM ()
movePlayer = moveActor playerId

moveActor :: ActorId -> Point -> HrogueM ()
moveActor actorId pdiff = do
  terrain <- gets hrougeStateTerrainMap
  let adjustActor actor =
        let
          prev = Actor.actorPosition actor
          next = prev `pointPlus` pdiff
          cell = terrainMapCell terrain next
        in if isWalkable cell then actor{ Actor.actorPosition = next } else actor
  modify' $ \state ->
    state{ hrougeStateActors = Map.adjust adjustActor actorId (hrougeStateActors state) }

left, right, up, down :: Point
left  = Point (-1) 0
right = Point 1 0
up    = Point 0 (-1)
down  = Point 0 1
