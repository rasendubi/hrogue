module Hrogue.Terminal
  ( Point(..)
  , clearScreen
  , withTerminal
  , putSymbol
  , getKey
  ) where

import Control.Exception (finally)
import System.IO (stdin, stdout, hSetBuffering, BufferMode(NoBuffering), hSetEcho, hReady)

import qualified System.Console.ANSI as ANSI

data Point = Point
  { pointX :: !Int
  , pointY :: !Int
  } deriving (Eq, Show)

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

putSymbol :: Point -> Char -> IO ()
putSymbol Point{ pointX=x, pointY=y } c = do
  ANSI.setCursorPosition y x
  putChar c

-- | Read key press from the stdin.
--
-- This might return more than one character in case of
-- escape-sequences.
getKey :: IO [Char]
getKey = reverse <$> getKey' []
  where
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      (if more then getKey' else return) (char:chars)
