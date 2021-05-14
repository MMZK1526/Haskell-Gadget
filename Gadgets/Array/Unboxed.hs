{-# LANGUAGE FlexibleContexts #-}

module Gadgets.Array.Unboxed where

import           Data.Array.Unboxed (IArray, UArray, bounds, listArray, (!))

-- | Making an array from a list, indexed from 0.
-- 
fromList :: IArray UArray a => [a] -> UArray Int a
fromList xs = listArray (0, length xs - 1) xs

-- | Safe array access.
-- 
infixr 4 !?
(!?) :: IArray UArray a => UArray Int a -> Int -> Maybe a
arr !? i 
  | i < inf || i > sup = Nothing
  | otherwise          = Just $ arr ! i
  where
     (inf, sup) = bounds arr
