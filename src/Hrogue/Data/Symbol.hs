{-# LANGUAGE NamedFieldPuns #-}
-- | Symbol is something displayable on the screen. (i.e., char +
-- color + attributes)
module Hrogue.Data.Symbol
  ( Symbol
  , Color
  , symbol
  , toText
  , withForeground
  , rgb
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Data.Word (Word8)

import qualified System.Console.ANSI as ANSI

data Symbol = Symbol
  { char       :: !Char
  , foreground :: !Color
  }
  deriving (Eq, Show)

newtype Color = Color { unColor :: Word8 }
  deriving (Eq, Show)

symbol :: Char -> Symbol
symbol c = Symbol c white

withForeground :: Color -> Symbol -> Symbol
withForeground color s = s{ foreground = color }

rgb :: Int -> Int -> Int -> Color
rgb r g b = Color $ ANSI.xterm6LevelRGB r g b

toText :: Symbol -> TL.Text
toText Symbol{ char, foreground } =
  -- avoid spawning codes if nothing changes
  if foreground == white
  then TL.singleton char
  else
    TL.fromChunks
      [ T.pack (ANSI.setSGRCode [colorToSgr foreground])
      , T.singleton char
      , resetCode
      ]

colorToSgr :: Color -> ANSI.SGR
colorToSgr = ANSI.SetPaletteColor ANSI.Foreground . unColor

resetCode :: T.Text
resetCode = T.pack $ ANSI.setSGRCode []

white :: Color
white = rgb 5 5 5
