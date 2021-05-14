module Gadgets.Array where

import           Data.Array (Array, bounds, listArray, (!))

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
