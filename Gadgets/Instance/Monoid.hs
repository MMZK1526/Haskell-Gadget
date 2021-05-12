-- With code excerpts from https://en.wikibooks.org/wiki/Haskell/Monoids.

module Gadgets.Instance.Monoid where

import           Control.Monad (liftM2)

import           Gadgets.Instance.Wrapper (Wrapper(..))

-- | Monoid under addition.
-- 
newtype Sum a = Sum { getSum :: a } deriving Show

-- | Monoid under multiplication.
-- 
newtype Product a = Product { getProduct :: a } deriving Show

instance Wrapper Sum where
  unwrap (Sum x) = x
  wrap           = Sum

instance Wrapper Product where
  unwrap (Product x) = x
  wrap               = Product

instance Num a => (Semigroup (Sum a)) where
  (<>) = liftM2 (+)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

instance Num a => (Semigroup (Product a)) where
  (<>) = liftM2 (*)

instance Num a => Monoid (Product a) where
  mempty = Product 1
