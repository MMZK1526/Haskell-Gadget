module Gadgets.Maybe where

import            Control.Monad (guard)

-- | Applies a predicate, returning "Nothing" if False.
toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe f a = guard (f a) >> Just a
{-# INLINE toMaybe #-}

-- | Run the monadic action if the first argument is a "Just".
onJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
onJust ma f = case ma of
  Just a  -> f a
  Nothing -> pure ()
{-# INLINE onJust #-}

-- | Run the monadic action if the first argument is "Nothing".
onNothing :: Applicative m => Maybe a -> m () -> m ()
onNothing ma f = case ma of
  Nothing -> f
  Just _  -> pure ()
{-# INLINE onNothing #-}
