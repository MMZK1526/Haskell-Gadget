{-# LANGUAGE ExistentialQuantification #-}

module Gadgets.Pure where

import           Control.Monad (ap, filterM, join)

data Misete = forall a. Show a => Misete a

instance Show Misete where
  show (Misete a) = show a

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

powerSet :: [a] -> [[a]]
powerSet = filterM $ const [True, False]

primes :: [Integer]
primes = 2 : filter
  (ap (all . ((0 /=) .) . mod)
      (flip takeWhile primes . (. join (*)) . flip (<=))) [3, 5..]
