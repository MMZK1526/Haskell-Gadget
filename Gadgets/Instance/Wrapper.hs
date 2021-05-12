{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Gadgets.Instance.Wrapper where

import          Control.Monad (ap, liftM)

class Wrapper w where
  unwrap :: w a -> a
  wrap   :: a -> w a

instance {-# OVERLAPPABLE #-} Wrapper w => Functor w where
  fmap = liftM

instance {-# OVERLAPPABLE #-} Wrapper w => Applicative w where
  pure  = return
  (<*>) = ap

instance {-# OVERLAPPABLE #-} Wrapper w => Monad w where
  return = wrap
  (>>=)  = flip (. unwrap)
