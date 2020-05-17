module Hrogue.Redraw (redraw) where

import           Control.Lens (at, use, (^.), _Just, (<<.=))

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

import           Hrogue.Data.Level (TerrainMap, isVisible, renderTerrain)
import           Hrogue.Data.Point (Point (Point))
import qualified Hrogue.Data.Symbol as Symbol

import qualified Hrogue.Types.Actor as Actor
import qualified Hrogue.Types.HrogueState as HrogueState

import           Hrogue.Terminal (goto)

import           Hrogue.Control.HrogueM


withVisibility :: Bool
withVisibility = False

redraw :: HrogueM ()
redraw = do
  level <- use HrogueState.terrainMap
  actors <- use HrogueState.actors

  mmessage <- HrogueState.message <<.= Nothing

  status <- statusLine

  liftIO $ do
    -- draw map
    goto (Point 0 1)
    ANSI.setSGR []
    TL.putStr $ renderMap level actors

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

renderMap :: TerrainMap -> Map.Map ActorId AnyActor -> TL.Text
renderMap level actors = finalText
  where
    currentPos = actors ^. at playerId . _Just . Actor.position

    terrain = renderTerrain level

    actorsMap = Map.fromList $ map (\a -> (a ^. Actor.position, a ^. Actor.symbol)) (Map.elems actors)

    visible :: (Int, Int) -> Bool
    visible (x, y) = isVisible currentPos (Point x y) level

    ifor = flip V.imap
    finalMap = ifor terrain $ \y row -> ifor row $ \x cell ->
      if not withVisibility || visible (x, y)
      then Symbol.toText $ fromMaybe cell (Map.lookup (Point x y) actorsMap)
      else TL.singleton ' '

    finalText = TL.intercalate (TL.singleton '\n') . V.toList . V.map (TL.concat . V.toList) $ finalMap

statusLine :: HrogueM T.Text
statusLine = do
  mactor <- use $ HrogueState.actor playerId
  return $ case mactor of
    Nothing    -> T.empty
    Just actor -> T.pack "HP:" <> T.pack (show $ actor ^. Actor.health)
