module Control.Lens.Polysemy where

import Polysemy (Member, Sem)
import Polysemy.State (State, gets, modify', put)

import Control.Lens (Lens', Getting, ASetter, ASetter', Over, view, (.~), (%~), (-~), (+~), LensLike, LensLike', set)

infix 4 .=, %%=, %=, <<.=, <%=, -=, +=, <-=, <+=

use :: (Member (State s) r) => Getting a s a -> Sem r a
use l = gets (view l)

uses :: (Member (State s) r) => Getting a s a -> (a -> b) -> Sem r b
uses l f = fmap f (use l)

assign :: (Member (State s) r) => ASetter s s a b -> b -> Sem r ()
assign l v = modify' $ set l v

(.=) :: (Member (State s) r) => ASetter s s a b -> b -> Sem r ()
l .= b = modify' (l .~ b)
{-# INLINE (.=) #-}

(%%=) :: (Member (State s) e) => Over p ((,) r) s s a b -> p a (r, b) -> Sem e r
l %%= f = do
  (r, s) <- gets (l f)
  put s
  return r
{-# INLINE (%%=) #-}

(%=) :: (Member (State s) r) => ASetter s s a b -> (a -> b) -> Sem r ()
l %= f = modify' $ l %~ f
{-# INLINE (%=) #-}

(<<.=) :: (Member (State a) r) => Lens' a b -> b -> Sem r b
l <<.= v = do
  r <- use l
  l .= v
  return r
{-# INLINE (<<.=) #-}

(<%=) :: (Member (State s) r) => LensLike ((,) b) s s a b -> (a -> b) -> Sem r b
l <%= f = l %%= (\b -> (b, b)) . f
{-# INLINE (<%=) #-}

(+=) :: (Member (State s) r, Num a) => ASetter' s a -> a -> Sem r ()
l += b = modify' (l +~ b)
{-# INLINE (+=) #-}

(-=) :: (Member (State s) r, Num a) => ASetter' s a -> a -> Sem r ()
l -= b = modify' (l -~ b)
{-# INLINE (-=) #-}

(<+=) :: (Member (State s) r, Num a) => LensLike' ((,) a) s a -> a -> Sem r a
l <+= a = l <%= (+ a)
{-# INLINE (<+=) #-}

(<-=) :: (Member (State s) r, Num a) => LensLike' ((,) a) s a -> a -> Sem r a
l <-= a = l <%= (subtract a)
{-# INLINE (<-=) #-}
