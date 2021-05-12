{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Gadgets.Instance.Wrapper where

import          Control.Monad (ap, liftM)
import          Control.Monad.Trans.Class (MonadTrans(..))

newtype WrapperT w m a = WrapperT { runWrapperT :: m (w a) }

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

instance {-# OVERLAPPABLE #-} (Monad m, Wrapper w) 
  => Functor (WrapperT w m) where
    fmap = liftM

instance {-# OVERLAPPABLE #-} (Monad m, Wrapper w) 
  => Applicative (WrapperT w m) where
    pure  = return
    (<*>) = ap

instance {-# OVERLAPPABLE #-} (Monad m, Wrapper w) 
  => Monad (WrapperT w m) where
    return = WrapperT . return . wrap
    (WrapperT w) >>= f 
      = WrapperT $ w >>= runWrapperT . f . unwrap

instance {-# OVERLAPPABLE #-} Wrapper w => MonadTrans (WrapperT w) where
  lift = WrapperT . fmap wrap
