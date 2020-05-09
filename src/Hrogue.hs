module Hrogue (run) where

import           Data.List                     (find)
import           Data.Maybe                    (listToMaybe)
import           System.IO                     (hPrint, hPutStrLn, stderr)

import           Control.Monad                 (forM, forM_, unless, void, when)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.State.Strict    (StateT (..), get, gets, modify',
                                                put)

import qualified Data.Map.Strict               as Map

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import qualified System.Console.ANSI           as ANSI
import qualified System.Console.ANSI.Types     as ANSI

import qualified System.Random                 as R
import qualified System.Random.Mersenne.Pure64 as MT

import qualified Algorithm.Search              as S

import           Hrogue.Data.Actor             (Actor)
import qualified Hrogue.Data.Actor             as Actor
import           Hrogue.Data.Level             (TerrainMap, isWalkable,
                                                parseMap, terrainMapCell,
                                                terrainMapStartPosition,
                                                terrainMapToString)
import           Hrogue.Data.Point             (Point (Point), pointMinus,
                                                pointPlus)
import           Hrogue.Terminal               (clearScreen, getKey, goto,
                                                putSymbol, withTerminal)

newtype ActorId = ActorId { unActorId :: Int }
  deriving (Eq, Ord, Show)

data HrogueState = HrogueState
    { hrogueStateTerrainMap :: !TerrainMap
    , hrogueStateActors     :: !(Map.Map ActorId Actor.Actor)
    , hrogueStateNextId     :: !ActorId
    , hrogueStateRng        :: !MT.PureMT
    , hrogueStateMessage    :: !(Maybe T.Text)
    }
    deriving (Show)

type HrogueM = StateT HrogueState IO

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

setMessage :: T.Text -> HrogueM ()
setMessage m = modify' $ \state -> state{ hrogueStateMessage = Just m }

getActor :: ActorId -> HrogueM (Maybe Actor)
getActor id = (Map.!? id) <$> gets hrogueStateActors

getActorUnsafe :: ActorId -> HrogueM Actor
getActorUnsafe id = (Map.! id) <$> gets hrogueStateActors

playerId = ActorId 0

snake :: Point -> Actor
snake position =
  Actor.Actor { Actor.actorType = Actor.Snake
              , Actor.actorPosition = position
              , Actor.actorSymbol  = 's'
              , Actor.actorSgr = [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
                                 , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green
                                 ]
              , Actor.actorHitpoints = 30
              }

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
                        , Actor.actorHitpoints = 100
                        }
          )
        , ( ActorId 1 , snake $ Point 77 9 )
        , ( ActorId 2 , snake $ Point 14 10 )
        ]
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
  forM_ mactor $ takeTurn actorId

takeTurn :: ActorId -> Actor -> HrogueM ()
takeTurn actorId actor = takeTurn' (Actor.actorType actor) actorId actor

takeTurn' :: Actor.ActorType -> ActorId -> Actor -> HrogueM ()
takeTurn' Actor.Player actorId actor = do
  k <- liftIO getKey
  processKey actorId k

takeTurn' Actor.Snake  actorId actor = do
  let currentPos = Actor.actorPosition actor
  terrain <- gets hrogueStateTerrainMap
  mplayer <- fmap Actor.actorPosition <$> getActor playerId
  let mnext = mplayer >>= \player -> do
        (price, path) <- searchPath currentPos player terrain
        return $ head path

  forM_ mnext $ \next ->
    moveActor actorId (next `pointMinus` currentPos)


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
    forM_ (Map.elems actors) Actor.displayActor

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
    Just actor -> T.pack "HP:" <> T.pack (show $ Actor.actorHitpoints actor)

processKey :: ActorId -> String -> HrogueM ()
processKey actorId k =
  let move = moveActor actorId
  in case k of
    "\ESC[A" -> move up
    "e"      -> move up
    "\ESC[B" -> move down
    "n"      -> move down
    "\ESC[C" -> move right
    "o"      -> move right
    "\ESC[D" -> move left
    "y"      -> move left
    "j"      -> move (up   `pointPlus` left)
    "f"      -> move (up   `pointPlus` right)
    "v"      -> move (down `pointPlus` left)
    "k"      -> move (down `pointPlus` right)
    _        -> return ()


moveActor :: ActorId -> Point -> HrogueM ()
moveActor actorId pdiff = do
  terrain <- gets hrogueStateTerrainMap
  actor <- getActorUnsafe actorId
  let prev = Actor.actorPosition actor
  let next = prev `pointPlus` pdiff
  let cell = terrainMapCell terrain next

  manotherActor <- actorAtPoint next

  case manotherActor of
    Just (bActorId, bActor) -> do
      let nextHitpoints = Actor.actorHitpoints bActor - 10
      if nextHitpoints <= 0
        then do
          deleteActor bActorId
          setMessage $ T.pack (show $ Actor.actorType bActor) <> T.pack " is killed"
        else do
          modifyActor bActorId $ const bActor{ Actor.actorHitpoints = nextHitpoints }
          when (actorId == playerId) $
            setMessage $ T.pack "You hit " <> T.pack (show $ Actor.actorType bActor)
          when (bActorId == playerId) $
            setMessage $ T.pack (show $ Actor.actorType actor) <> T.pack " hits you"
    Nothing ->
      when (isWalkable cell) $
        modifyActor actorId $ \a -> a{ Actor.actorPosition = next }

modifyActor :: ActorId -> (Actor -> Actor) -> HrogueM ()
modifyActor actorId f =
  modify' $ \state -> state{ hrogueStateActors = Map.adjust f actorId (hrogueStateActors state) }

deleteActor :: ActorId -> HrogueM ()
deleteActor actorId =
  modify' $ \state -> state{ hrogueStateActors = Map.delete actorId (hrogueStateActors state) }

actorAtPoint :: Point -> HrogueM (Maybe (ActorId, Actor))
actorAtPoint target = do
  actors <- gets hrogueStateActors
  return $ find (\(_, Actor.Actor{ Actor.actorPosition = p }) -> p == target) . Map.toList $ actors

left, right, up, down :: Point
left  = Point (-1) 0
right = Point 1 0
up    = Point 0 (-1)
down  = Point 0 1

directions = [ left
             , right
             , up
             , down
             , left <> up
             , left <> down
             , right <> up
             , right <> down
             ]

searchPath :: Point -> Point -> TerrainMap -> Maybe (Int, [Point])
searchPath from to map = S.aStar (next `S.pruning` isWall) cost estimate (== to) from
  where
    isWall = not . isWalkable . terrainMapCell map
    next p = fmap (p <>) directions
    cost (Point x1 y1) (Point x2 y2) = 1 -- all neighboring cells have cost of 1
    estimate (Point x1 y1) = abs (x1 - x2) + abs (y1 - y2)
      where (Point x2 y2) = to
