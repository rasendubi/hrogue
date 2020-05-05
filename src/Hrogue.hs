module Hrogue (run) where

import           Control.Monad              (unless, void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT (..), get, gets, modify',
                                             put)
import           Data.Text.IO               as T

import           Hrogue.Data.Level          (TerrainMap, parseMap,
                                             terrainMapStartPosition,
                                             terrainMapToString)
import           Hrogue.Terminal            (Point (Point), clearScreen, getKey,
                                             goto, pointPlus, putSymbol,
                                             withTerminal)

data HrogueState = HrogueState
    { hrougeStatePlayer :: !Point
    , terrainMap        :: !TerrainMap
    }
    deriving (Show)

type HrogueM = StateT HrogueState IO

run :: IO ()
run = withTerminal $ do
  level <- parseMap <$> T.readFile "data/level.txt"
  let initialState = HrogueState (terrainMapStartPosition level) level
  void $ runStateT game initialState

game :: HrogueM ()
game = do
  redraw
  k <- liftIO getKey
  processKey k
  unless (k == "q") game

redraw :: HrogueM ()
redraw = do
  level <- gets terrainMap
  player <- gets hrougeStatePlayer
  liftIO $ do
    clearScreen
    goto (Point 0 0)
    T.putStr (terrainMapToString level)
    putSymbol player '@'

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
movePlayer (Point diffX diffY) = modify' $ \state ->
  let (Point playerX playerY) = hrougeStatePlayer state
  in state{ hrougeStatePlayer = Point (playerX + diffX) (playerY + diffY) }

left, right, up, down :: Point
left  = Point (-1) 0
right = Point 1 0
up    = Point 0 (-1)
down  = Point 0 1
