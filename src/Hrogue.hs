module Hrogue (run) where

import           Control.Lens (Lens', singular, (&), (^.), _Just)
import           Control.Lens.Polysemy (use, (-=), (.=), (<+=))

import           Control.Monad (forM_, void, when)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.Map.Strict as Map

import qualified Data.Text as T

import qualified System.Random.Mersenne.Pure64 as MT

import           Hrogue.Data.Level (terrainMapSize, terrainMapStartPosition)
import           Hrogue.Data.Point (Point (Point))

import           Hrogue.Terminal (withTerminal)

import qualified Hrogue.Types.Actor as Actor
import qualified Hrogue.Types.HrogueState as HrogueState

import qualified Hrogue.Types.Action as Action

import           Hrogue.Actor.Player as Player
import qualified Hrogue.Actor.Snake as Snake

import           Hrogue.LevelGen (generateLevel)

import           Hrogue.Control.HrogueM
import           Hrogue.Types.Internal (HrogueM)

snake :: ActorId -> Point -> AnyActor
snake actorId position = AnyActor $ Snake.mkSnake actorId position

player :: ActorId -> Point -> (Int, Int) -> AnyActor
player actorId position sizes = AnyActor $ Player.mkPlayer actorId position sizes

run :: IO ()
run = withTerminal $ do
  rng <- liftIO MT.newPureMT
  let (level, rng') = generateLevel (80, 24) rng
  let sizes = terrainMapSize level
  let actorList =
        [ player playerId (terrainMapStartPosition level) sizes
        -- , snake (ActorId 1) (Point 77 9)
        -- , snake (ActorId 2) (Point 14 10)
        ]
  let actors = Map.fromList $ fmap (\a -> (a ^. Actor.actorId, a)) actorList
  let initialState = HrogueState
        { HrogueState._terrainMap = level
        , HrogueState._actors = actors
        , HrogueState._message = Just $ T.pack "Welcome to hrogue!"
        }
  void $ runHrogueM rng' initialState game

game :: HrogueM ()
game = tick >> game

tick :: HrogueM ()
tick = do
  actors <- use HrogueState.actors
  forM_ (Map.keys actors) maybeTakeTurn

-- additional check that actor didn't just died from previous actor's
-- move
maybeTakeTurn :: ActorId -> HrogueM ()
maybeTakeTurn actorId = do

  mactor <- use (HrogueState.actor actorId)
  forM_ mactor $ \actor -> do
    let
      energy :: Lens' HrogueState Int
      energy = HrogueState.actor actorId . singular _Just . Actor.energy

    energy' <- energy <+= actor ^. Actor.speed

    when (energy' > 0) $ do
      (actor', action) <- Actor.withActor actor Actor.takeTurn
      HrogueState.actor actorId .= Just actor'
      actor' & action ^. Action.run
      energy -= action ^. Action.cost
