{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Gadgets.Instance.Wrapper where

import          Control.Monad (ap, liftM)

class Wrapper w where
  unwrap :: w a -> a
  wrap   :: a -> w a

instance Wrapper w => Functor w where
  fmap = liftM

instance Wrapper w => Applicative w where
  pure  = return
  (<*>) = ap

instance Wrapper w => Monad w where
  return = wrap
  (>>=)  = flip (. unwrap)
