module Hrogue.Terminal
  ( clearScreen
  , withTerminal
  , putSymbol
  , getKey
  , goto
  ) where

import           Control.Exception   (finally)
import           System.IO           (BufferMode (NoBuffering), hReady,
                                      hSetBuffering, hSetEcho, stdin, stdout)

import qualified System.Console.ANSI as ANSI

import           Hrogue.Data.Point   (Point (Point))

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
goto (Point x y) = ANSI.setCursorPosition y x

putSymbol :: Point -> [ANSI.SGR] -> Char -> IO ()
putSymbol p sgr c = do
  goto p
  ANSI.setSGR sgr
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
