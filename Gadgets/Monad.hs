{-# LANGUAGE FlexibleInstances #-}

module Gadgets.Monad where

import           Control.Applicative (liftA2)
import           Control.Monad.Trans.Writer (WriterT, censor)

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

-- | Clear the log of a @WriterT@
-- 
clear :: (Monoid w, Monad m) => WriterT w m a -> WriterT w m a
clear = censor $ const mempty

-- | For monads, @ void_ = return () @.
-- 
void_ :: Applicative f => f ()
void_ = pure ()
