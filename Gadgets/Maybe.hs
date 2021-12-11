module Gadgets.Maybe where

-- | Applies a predicate, returning "Nothing" if False.
toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe f a
  | f a       = Just a
  | otherwise = Nothing
