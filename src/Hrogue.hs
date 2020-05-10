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
import           Hrogue.Data.Level             (parseMap,
                                                terrainMapStartPosition,
                                                terrainMapToString)
import           Hrogue.Data.Point             (Point (Point))
import           Hrogue.Terminal               (goto, withTerminal)

import           Hrogue.Data.Actor.Player      (Player (Player))
import           Hrogue.Data.Actor.Snake       (Snake (Snake))

import           Hrogue.Control.HrogueM

hrogueRndInt :: HrogueM Int
hrogueRndInt = do
  rng <- gets hrogueStateRng
  let (result, rng') = MT.randomInt rng
  modify' $ \state -> state{ hrogueStateRng = rng' }
  return result

hrogueRndRange :: R.Random a => (a, a) -> HrogueM a
hrogueRndRange range = do
  rng <- gets hrogueStateRng
  let (result, rng') = R.randomR range rng
  modify' $ \state -> state{ hrogueStateRng = rng' }
  return result

snake :: ActorId -> Point -> AnyActor
snake actorId position = AnyActor $
  Actor.Actor { Actor.actorId = actorId
              , Actor.actorPosition = position
              , Actor.actorSymbol  = 's'
              , Actor.actorSgr = [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
                                 , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green
                                 ]
              , Actor.actorHitpoints = 30
              , Actor.actorState = Snake
              }

run :: IO ()
run = withTerminal $ do
  level <- parseMap <$> T.readFile "data/level.txt"
  let actorList =
        [ AnyActor $
            Actor.Actor { Actor.actorId = playerId
                        , Actor.actorPosition = terrainMapStartPosition level
                        , Actor.actorSymbol = '@'
                        , Actor.actorSgr = [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
                                           , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan
                                           ]
                        , Actor.actorHitpoints = 100
                        , Actor.actorState = Player
                        }
        , snake (ActorId 1) (Point 77 9)
        , snake (ActorId 2) (Point 14 10)
        ]
  let actors = Map.fromList $ fmap (\any@(AnyActor a) -> (Actor.actorId a, any)) actorList
  rng <- liftIO MT.newPureMT
  let initialState = HrogueState { hrogueStateTerrainMap = level
                                 , hrogueStateActors = actors
                                 , hrogueStateNextId = ActorId 3
                                 , hrogueStateRng = rng
                                 , hrogueStateMessage = Just $ T.pack "Welcome to hrogue!"
                                 }
  void $ runStateT game initialState


game :: HrogueM ()
game = do
  redraw
  tick
  game

tick :: HrogueM ()
tick = do
  actors <- gets hrogueStateActors
  forM_ (Map.keys actors) maybeTakeTurn

-- additional check that actor didn't just died from previous actor's
-- move
maybeTakeTurn :: ActorId -> HrogueM ()
maybeTakeTurn actorId = do
  mactor <- getActor actorId
  forM_ mactor $ \(AnyActor actor) -> actorTakeTurn actor

--
-- takeTurn' Actor.Snake  actorId actor = do
--   let currentPos = Actor.actorPosition actor
--   terrain <- gets hrogueStateTerrainMap
--   mplayer <- fmap Actor.actorPosition <$> getActor playerId
--   let mnext = mplayer >>= \player -> do
--         (price, path) <- searchPath currentPos player terrain
--         return $ head path
--
--   forM_ mnext $ \next ->
--     moveActor actorId (next `pointMinus` currentPos)


redraw :: HrogueM ()
redraw = do
  level <- gets hrogueStateTerrainMap
  actors <- gets hrogueStateActors
  mmessage <- gets hrogueStateMessage

  modify' $ \state -> state{ hrogueStateMessage = Nothing }

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
    Just (AnyActor actor) -> T.pack "HP:" <> T.pack (show $ Actor.actorHitpoints actor)
