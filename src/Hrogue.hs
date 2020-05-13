module Hrogue (run) where

import           Control.Lens                  (use, (.=), (^.))

import           Control.Monad                 (forM_, void)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.State.Strict    (StateT (..))

import qualified Data.Map.Strict               as Map

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import qualified System.Console.ANSI           as ANSI

import qualified System.Random.Mersenne.Pure64 as MT

import           Hrogue.Data.Level             (parseMap,
                                                terrainMapStartPosition,
                                                terrainMapToString)
import           Hrogue.Data.Point             (Point (Point))
import           Hrogue.Terminal               (goto, withTerminal)

import qualified Hrogue.Types.Actor            as Actor
import qualified Hrogue.Types.HrogueState      as HrogueState

import           Hrogue.Actor.Player           as Player
import qualified Hrogue.Actor.Snake            as Snake

import           Hrogue.Control.HrogueM


snake :: ActorId -> Point -> AnyActor
snake actorId position = AnyActor $ Snake.mkSnake actorId position

player :: ActorId -> Point -> AnyActor
player actorId position =
  AnyActor $
    Player.Player $
      Actor.BaseActor
        { Actor._actorId = actorId
        , Actor._name = T.pack "Player"
        , Actor._position = position
        , Actor._symbol = '@'
        , Actor._sgr =
            [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
            , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan
            ]
        , Actor._health = 100
        }

run :: IO ()
run = withTerminal $ do
  level <- parseMap <$> T.readFile "data/level.txt"
  let actorList =
        [ player playerId (terrainMapStartPosition level)
        , snake (ActorId 1) (Point 77 9)
        , snake (ActorId 2) (Point 14 10)
        ]
  let actors = Map.fromList $ fmap (\a -> (a ^. Actor.actorId, a)) actorList
  rng <- liftIO MT.newPureMT
  let initialState = HrogueState
        { HrogueState._terrainMap = level
        , HrogueState._actors = actors
        , HrogueState._nextId = ActorId 3
        , HrogueState._rng = rng
        , HrogueState._message = Just $ T.pack "Welcome to hrogue!"
        }
  void $ runStateT game initialState

game :: HrogueM ()
game = do
  redraw
  tick
  game

tick :: HrogueM ()
tick = do
  actors <- use HrogueState.actors
  forM_ (Map.keys actors) maybeTakeTurn

-- additional check that actor didn't just died from previous actor's
-- move
maybeTakeTurn :: ActorId -> HrogueM ()
maybeTakeTurn actorId = do
  actors <- use (HrogueState.actor actorId)
  forM_ actors (Actor.takeTurn :: AnyActor -> HrogueM ())

redraw :: HrogueM ()
redraw = do
  level <- use HrogueState.terrainMap
  actors <- use HrogueState.actors
  mmessage <- use HrogueState.message

  HrogueState.message .= Nothing

  status <- statusLine

  liftIO $ do
    -- draw map
    goto (Point 0 0)
    ANSI.setSGR []
    T.putStr (terrainMapToString level)

    -- draw actors
    forM_ (Map.elems actors) displayActor

    -- display message
    goto (Point 0 0)
    ANSI.setSGR []
    forM_ mmessage T.putStr
    ANSI.clearFromCursorToLineEnd

    -- display status bar
    goto (Point 0 24)
    ANSI.setSGR []
    T.putStr status
    ANSI.clearFromCursorToLineEnd


statusLine :: HrogueM T.Text
statusLine = do
  mactor <- use $ HrogueState.actor playerId
  return $ case mactor of
    Nothing    -> T.empty
    Just actor -> T.pack "HP:" <> T.pack (show $ actor ^. Actor.health)
