{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Gadgets.Instance.Monad where

import           Control.Applicative (liftA2)
import           Control.Monad.Fail (MonadFail)
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

instance {-# OVERLAPPABLE #-} Monad m => MonadFail m where
  fail = error
