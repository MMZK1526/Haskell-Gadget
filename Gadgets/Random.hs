module Gadgets.Random where

import           Control.Monad.Trans.State (StateT, get, put)
import           System.Random (Random, RandomGen, random, randomR)

-- | "random" wrapped in "StateT".
randomT :: (Monad m, Random a, RandomGen g) => StateT g m a
randomT = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r

-- | "randomR" wrapped in "StateT".
randomRT :: (Monad m, Random a, RandomGen g) => (a, a) -> StateT g m a
randomRT range = do
  gen <- get
  let (r, gen') = randomR range gen
  put gen'
  return r
