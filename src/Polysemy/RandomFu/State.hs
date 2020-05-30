{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
-- | RandomFu effect interpreter that reinterprets RandomFu effect in
-- terms of (State PureMT) effect.
--
-- This is useful, so I can access internal state of PureMT—for
-- example, to persist it.
module Polysemy.RandomFu.State
  ( PureMT, newPureMT, pureMT
  , runRandomWithState
  ) where

import           Data.Random (sample)
import           Data.Random.Internal.Source (getRandomPrim)
import           Data.Random.Source
    (MonadRandom, getRandomDouble, getRandomWord64, monadRandom)

import           Polysemy (Member, Sem, interpret)
import           Polysemy.RandomFu (RandomFu (GetRandomPrim, SampleRVar))
import           Polysemy.State (State, get, put)

import           System.Random.Mersenne.Pure64
    (PureMT, newPureMT, pureMT, randomDouble, randomWord64)

withStateE :: Member (State PureMT) r => (PureMT -> (t, PureMT)) -> Sem r t
withStateE thing = do
  mt <- get
  let (ws, newMt) = thing mt
  put newMt
  return ws

-- TODO: This is currently an orphan instance. Wrapping in a newtype
-- might be a good idea.
$(monadRandom
   [d| instance Member (State PureMT) r => MonadRandom (Sem r) where
           getRandomWord64 = withStateE randomWord64
           getRandomDouble = withStateE randomDouble
   |])

runRandomWithState :: (Member (State PureMT) r) => Sem (RandomFu ': r) a -> Sem r a
runRandomWithState = interpret $ \case
  SampleRVar    rv -> sample rv
  GetRandomPrim pt -> getRandomPrim pt
