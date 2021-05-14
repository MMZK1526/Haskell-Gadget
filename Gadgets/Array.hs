module Gadgets.Array where

import           Data.Array (Array, bounds, listArray, (!), (//))

-- | Making an array from a list, indexed from 0.
-- 
fromList :: [a] -> Array Int a
fromList xs = listArray (0, length xs - 1) xs

-- | Safe array access.
-- 
infixr 4 !?
(!?) :: Array Int a -> Int -> Maybe a
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
(=:) :: Array Int a -> Int -> a -> Array Int a
(=:) arr i e 
  | i < inf || i > sup = arr 
  | otherwise          = arr // [(i, e)]
  where
     (inf, sup) = bounds arr
