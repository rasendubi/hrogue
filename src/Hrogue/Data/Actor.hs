module Hrogue.Data.Actor
  ( ActorType(..)
  , Actor(..)
  , displayActor
  ) where

import qualified System.Console.ANSI as ANSI

import qualified Hrogue.Terminal     as Terminal

import           Hrogue.Data.Point   (Point (Point))

data ActorType = Player
    | Snake
    deriving (Eq, Show)

data Actor = Actor
    { actorType     :: !ActorType
    , actorPosition :: !Point
    , actorSymbol   :: !Char
    , actorSgr      :: ![ANSI.SGR]
    }
    deriving (Eq, Show)

displayActor :: Actor -> IO ()
displayActor a = Terminal.putSymbol (actorPosition a) (actorSgr a) (actorSymbol a)
