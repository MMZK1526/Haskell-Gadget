{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Gadgets.RAList (
  RAList(Empty, (:<)), empty, fromList, head, modify, singleton, tail, toList,
  update, update', (!), (!?), (><)
) where

import           Gadgets.RAList.Internal 
  (RAList(..), empty, fromList, head, modify, singleton, tail, toList, update, 
  update', (!), (!?), (><))
import           Gadgets.RAList.IsList ()
import           Prelude hiding (head, tail)

instance Functor RAList where
  fmap _ Empty     = Empty
  fmap f (x :< xs) = f x :< fmap f xs

instance Applicative RAList where
  pure = (:< Empty)

  Empty <*> _      = Empty
  (f :< fs) <*> xs = fmap f xs >< (fs <*> xs)

instance Semigroup (RAList e) where
  (<>) = (><)

instance Monoid (RAList e) where
  mempty = Empty
