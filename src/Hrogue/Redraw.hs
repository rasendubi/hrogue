module Hrogue.Redraw (redraw) where

import           Polysemy (Embed, Member, Sem)
import           Polysemy.State (State)

import           Control.Lens ((^.))
import           Control.Lens.Polysemy (use, (<<.=))

import           Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as Map

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import qualified Data.Vector as V

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)

import qualified System.Console.ANSI as ANSI

import           Hrogue.Data.Level
    (TerrainCell (..), TerrainMap, cellToSymbol, unTerrainMap)
import           Hrogue.Data.Point (Point (Point))
import qualified Hrogue.Data.Symbol as Symbol

import qualified Hrogue.Types.Actor as Actor
import qualified Hrogue.Types.HrogueState as HrogueState

import           Hrogue.Terminal (goto)

import           Hrogue.Control.HrogueM

withVisibility :: Bool
withVisibility = True

redraw :: (Member (State HrogueState.HrogueState) r, Member (Embed IO) r) => V.Vector (V.Vector Bool) -> V.Vector (V.Vector Bool) -> Sem r ()
redraw knownMap visibilityMap = do
  level <- use HrogueState.terrainMap
  actors <- use HrogueState.actors

  mmessage <- HrogueState.message <<.= Nothing

  status <- statusLine

  liftIO $ do
    -- draw map
    goto (Point 0 1)
    ANSI.setSGR []
    TL.putStr $ renderMap level actors knownMap visibilityMap

    -- display message
    goto (Point 0 0)
    ANSI.setSGR []
    forM_ mmessage T.putStr
    ANSI.clearFromCursorToLineEnd

    -- display status bar
    goto (Point 0 25)
    ANSI.setSGR []
    T.putStr status
    ANSI.clearFromCursorToLineEnd

renderMap :: TerrainMap -> Map.Map ActorId AnyActor -> V.Vector (V.Vector Bool) -> V.Vector (V.Vector Bool) -> TL.Text
renderMap level actors knownMap visibilityMap = finalText
  where
    terrain = unTerrainMap level

    actorsMap = Map.fromList $ map (\a -> (a ^. Actor.position, a ^. Actor.symbol)) (Map.elems actors)

    finalMap = V.izipWith3 (\y -> V.izipWith3 $ \x km v t -> renderCell km v (x, y) t) knownMap visibilityMap terrain

    renderCell :: Bool -> Bool -> (Int, Int) -> TerrainCell -> Symbol.Symbol
    -- not known
    renderCell False _ _ _ | withVisibility = Symbol.symbol ' '
    -- not visible
    renderCell _ False _ cell | withVisibility = Symbol.withForeground (Symbol.rgb 1 1 1) $ cellToSymbol cell
    -- visible
    renderCell _ _ (x, y) cell = fromMaybe (cellToSymbol cell) (Map.lookup (Point x y) actorsMap)

    finalText =
      TL.intercalate (TL.singleton '\n') $
        V.toList $ V.map (TL.concat . fmap Symbol.toText . V.toList) finalMap

statusLine :: (Member (State HrogueState.HrogueState) r) => Sem r T.Text
statusLine = do
  mactor <- use $ HrogueState.actor playerId
  return $ case mactor of
    Nothing    -> T.empty
    Just actor -> T.pack "HP:" <> T.pack (show $ actor ^. Actor.health)
