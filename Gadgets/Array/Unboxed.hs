{-# LANGUAGE FlexibleContexts #-}

module Gadgets.Array.Unboxed where

import           Data.Array.Unboxed 
  (IArray, UArray, bounds, listArray, (!), (//))

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

-- | Update a value in the array.
-- Example: @ arr =: 3 $ 5 @ sets the third element to five.
-- It will produce the same array if the index is out of bound.
-- 
infixl 3 =:
(=:) :: IArray UArray a => UArray Int a -> Int -> a -> UArray Int a
(=:) arr i e 
  | i < inf || i > sup = arr 
  | otherwise          = arr // [(i, e)]
  where
     (inf, sup) = bounds arr
