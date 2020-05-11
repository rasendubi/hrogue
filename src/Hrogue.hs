module Hrogue (run) where

import Control.Lens ((^.), (.=), use)

import           Control.Monad                 (forM_, void)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.State.Strict    (StateT (..))

import qualified Data.Map.Strict               as Map

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import qualified System.Console.ANSI           as ANSI

import qualified System.Random.Mersenne.Pure64 as MT

import qualified Hrogue.Data.Actor             as Actor
import qualified Hrogue.Data.HrogueState       as HrogueState
import           Hrogue.Data.Level             (parseMap,
                                                terrainMapStartPosition,
                                                terrainMapToString)
import           Hrogue.Data.Point             (Point (Point))
import           Hrogue.Terminal               (goto, withTerminal)

import           Hrogue.Data.Actor.Player      (Player (Player))
import           Hrogue.Data.Actor.Snake       (Snake (Snake))

import           Hrogue.Control.HrogueM


snake :: ActorId -> Point -> ActorWithAnyState
snake actorId position = ActorWithState
  Actor.Actor
    { Actor._actorId = actorId
    , Actor._position = position
    , Actor._symbol  = 's'
    , Actor._sgr =
      [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
      , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green
      ]
    , Actor._hitpoints = 30
    }
  (AnyActorState Snake)

player :: ActorId -> Point -> ActorWithAnyState
player actorId position = ActorWithState
  Actor.Actor
    { Actor._actorId = actorId
    , Actor._position = position
    , Actor._symbol = '@'
    , Actor._sgr =
        [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
        , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan
        ]
    , Actor._hitpoints = 100
    }
  (AnyActorState Player)

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
maybeTakeTurn actorId = use (HrogueState.actor actorId) >>= mapM_ actorTakeTurn

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
    Just actor -> T.pack "HP:" <> T.pack (show $ actor ^. Actor.hitpoints)
