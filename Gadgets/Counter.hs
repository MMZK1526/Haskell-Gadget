module Gadgets.Counter where

import qualified Data.Map as M

type Counter a = M.Map a Integer

-- | Add the value at the key by n.
addN :: Ord a => a -> Integer -> Counter a -> Counter a
addN e n = M.insertWith (const (+ n)) e n

-- | Remove the key from the "Counter".
clear :: Ord a => a -> Counter a -> Counter a
clear = M.delete

-- | The empty "Counter".
empty :: Counter a
empty = M.empty

-- | Generates a "Counter" from a list.
fromList :: Ord a => [a] -> Counter a
fromList = foldr inc empty

-- | Increment the value at the key.
inc :: Ord a => a -> Counter a -> Counter a
inc = flip (M.insertWith (const (+ 1))) 1

-- | Convert a "Counter" to pairs of elements and counts.
toPairList :: Counter a -> [(a, Integer)]
toPairList = M.toList
