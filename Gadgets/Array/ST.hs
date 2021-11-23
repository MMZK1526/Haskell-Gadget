{-# LANGUAGE FlexibleContexts #-}

module Gadgets.Array.ST where

import           Control.Monad.ST (ST)
import           Data.Array (Array)
import qualified Data.Array.ST as A
import qualified Data.Array.Unsafe as A
import           Data.Maybe (fromJust)

type STArray = A.STArray

-- | This is the same as the default @freeze@ function, but it has specified
-- type to avoid explicit signature binding.
--
freeze :: STArray s Int a -> ST s (Array Int a)
freeze = A.freeze

-- | This is the same as the default @thaw@ function, but it has specified type
-- to avoid explicit signature binding.
--
thaw :: Array Int a -> ST s (STArray s Int a)
thaw = A.thaw

-- | This is the same as the default @unsafeFreeze@ function, but it has
-- specified type to avoid explicit signature binding.
--
unsafeFreeze :: STArray s Int a -> ST s (Array Int a)
unsafeFreeze = A.unsafeFreeze

-- | This is the same as the default @unsafeThaw@ function, but it has specified
-- type to avoid explicit signature binding.
--
unsafeThaw :: Array Int a -> ST s (STArray s Int a)
unsafeThaw = A.unsafeThaw
