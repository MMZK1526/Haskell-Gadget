{-# LANGUAGE FlexibleInstances #-}

module Gadgets.Monad where

import           Control.Applicative (liftA2)

instance {-# OVERLAPPABLE #-} (Num a, Applicative m) => Num (m a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

instance {-# OVERLAPPABLE #-} (Fractional a, Applicative m)
  => Fractional (m a) where
    (/)          = liftA2 (/)
    fromRational = pure . fromRational


-- | For monads, @ void_ = return () @.
-- 
void_ :: Applicative f => f ()
void_ = pure ()
