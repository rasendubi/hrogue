module Hrogue (run) where

import           Control.Monad                 (forM_, void)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.State.Strict    (StateT (..), gets, modify')

import qualified Data.Map.Strict               as Map

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import qualified System.Console.ANSI           as ANSI

import qualified System.Random                 as R
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

_hrogueRndInt :: HrogueM Int
_hrogueRndInt = do
  rng <- gets HrogueState.rng
  let (result, rng') = MT.randomInt rng
  modify' $ \state -> state{ HrogueState.rng = rng' }
  return result

_hrogueRndRange :: R.Random a => (a, a) -> HrogueM a
_hrogueRndRange range = do
  rng <- gets HrogueState.rng
  let (result, rng') = R.randomR range rng
  modify' $ \state -> state{ HrogueState.rng = rng' }
  return result

snake :: ActorId -> Point -> AnyActor
snake actorId position = AnyActor $
  Actor.Actor
    { Actor.id = actorId
    , Actor.position = position
    , Actor.symbol  = 's'
    , Actor.sgr =
      [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
      , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green
      ]
    , Actor.hitpoints = 30
    , Actor.state = Snake
    }

run :: IO ()
run = withTerminal $ do
  level <- parseMap <$> T.readFile "data/level.txt"
  let actorList =
        [ AnyActor $
            Actor.Actor
              { Actor.id = playerId
              , Actor.position = terrainMapStartPosition level
              , Actor.symbol = '@'
              , Actor.sgr =
                [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
                , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan
                ]
              , Actor.hitpoints = 100
              , Actor.state = Player
              }
        , snake (ActorId 1) (Point 77 9)
        , snake (ActorId 2) (Point 14 10)
        ]
  let actors = Map.fromList $ fmap (\actor@(AnyActor a) -> (Actor.id a, actor)) actorList
  rng <- liftIO MT.newPureMT
  let initialState = HrogueState
        { HrogueState.terrainMap = level
        , HrogueState.actors = actors
        , HrogueState.nextId = ActorId 3
        , HrogueState.rng = rng
        , HrogueState.message = Just $ T.pack "Welcome to hrogue!"
        }
  void $ runStateT game initialState


game :: HrogueM ()
game = do
  redraw
  tick
  game

tick :: HrogueM ()
tick = do
  actors <- gets HrogueState.actors
  forM_ (Map.keys actors) maybeTakeTurn

-- additional check that actor didn't just died from previous actor's
-- move
maybeTakeTurn :: ActorId -> HrogueM ()
maybeTakeTurn actorId = do
  mactor <- getActor actorId
  forM_ mactor $ \(AnyActor actor) -> actorTakeTurn actor

redraw :: HrogueM ()
redraw = do
  level <- gets HrogueState.terrainMap
  actors <- gets HrogueState.actors
  mmessage <- gets HrogueState.message

  modify' $ \state -> state{ HrogueState.message = Nothing }

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
  mactor <- getActor playerId
  return $ case mactor of
    Nothing    -> T.empty
    Just (AnyActor actor) -> T.pack "HP:" <> T.pack (show $ Actor.hitpoints actor)
