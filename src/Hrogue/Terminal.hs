module Hrogue.Terminal
  ( Point(..)
  , pointPlus
  , clearScreen
  , withTerminal
  , putSymbol
  , getKey
  , goto
  ) where

import Control.Exception (finally)
import System.IO (stdin, stdout, hSetBuffering, BufferMode(NoBuffering), hSetEcho, hReady)

import qualified System.Console.ANSI as ANSI

data Point = Point
  { pointX :: !Int
  , pointY :: !Int
  } deriving (Eq, Show)

pointPlus :: Point -> Point -> Point
pointPlus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

clearScreen :: IO ()
clearScreen = ANSI.clearScreen

prepareTerminal :: IO ()
prepareTerminal = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  ANSI.clearScreen
  ANSI.hideCursor

restoreTerminal :: IO ()
restoreTerminal = do
  ANSI.clearScreen
  ANSI.showCursor
  hSetEcho stdin True

withTerminal :: IO () -> IO ()
withTerminal f = finally (prepareTerminal >> f) restoreTerminal

goto :: Point -> IO ()
goto (Point x y) = do
  ANSI.setCursorPosition y x

putSymbol :: Point -> Char -> IO ()
putSymbol p c = do
  goto p
  putChar c

-- | Read key press from the stdin.
--
-- This might return more than one character in case of
-- escape-sequences.
getKey :: IO String
getKey = reverse <$> getKey' []
  where
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      (if more then getKey' else return) (char:chars)