module Hrogue (run) where

import Control.Monad (void, unless)
import Control.Monad.State.Lazy (StateT(..), get, gets, put, modify')
import Control.Monad.IO.Class (liftIO)

import Hrogue.Terminal (Point(Point), withTerminal, putSymbol, getKey, clearScreen)

data HrogueState = HrogueState
  { hrougeStatePlayer :: !Point
  } deriving (Show)

type HrogueM = StateT HrogueState IO

run :: IO ()
run = withTerminal $ do
  let initialState = HrogueState (Point 0 0)
  void $ runStateT game initialState

game :: HrogueM ()
game = do
  redraw
  k <- liftIO getKey
  processKey k
  unless (k == "q") game

redraw :: HrogueM ()
redraw = do
  liftIO $ clearScreen
  player <- gets hrougeStatePlayer
  liftIO $ putSymbol player '@'

processKey :: [Char] -> HrogueM ()
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
