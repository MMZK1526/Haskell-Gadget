{-# LANGUAGE FlexibleContexts #-}

module Gadgets.Array.Unboxed where

import           Control.Monad (ap)
import           Data.Array.Unboxed 
  (IArray, Ix, UArray, array, bounds, inRange, listArray, (!), (//))

-- | Making an array from a list, indexed from 0.
-- 
fromList :: IArray UArray a => [a] -> UArray Int a
fromList xs = listArray (0, length xs - 1) xs

-- | Making a row-major 2D array from a list, indexed from (0, 0).
-- Will err on non-rectangular inputs.
from2DListR :: IArray UArray a => [[a]] -> UArray (Int, Int) a
from2DListR xz = array ((0, 0), (length xz - 1, length (head xz) - 1)) $ concat
               $ zipWith (flip zipWith [0..] . (((,) .) . (,))) [0..] xz

-- | Making a column-major 2D array from a list, indexed from (0, 0).
-- Will err on non-rectangular inputs.
from2DListC :: IArray UArray a => [[a]] -> UArray (Int, Int) a
from2DListC xz = array ((0, 0), (length (head xz) - 1, length xz - 1)) $ concat
               $ zipWith (flip zipWith [0..] . flip (((,) .) . (,))) [0..] xz

-- | Adjusts a value in the array with the given function.
-- It will do nothing if the index is out of bound.
adjust :: (Ix i, IArray UArray a) => UArray i a -> (a -> a) -> i -> UArray i a
adjust arr f i = arr // [(i, f $ arr ! i)]

-- | Strict version of "adjust".
adjust' :: (Ix i, IArray UArray a) => UArray i a -> (a -> a) -> i -> UArray i a
adjust' = (. ap seq) . adjust

-- | Safe array access.
-- 
infixr 4 !?
(!?) :: (Ix i, IArray UArray a) => UArray i a -> i -> Maybe a
arr !? i 
  | inRange (bounds arr) i = Just $ arr ! i
  | otherwise              = Nothing
