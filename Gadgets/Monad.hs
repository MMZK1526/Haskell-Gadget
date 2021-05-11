{-# LANGUAGE FlexibleInstances #-}

module Gadgets.Monad where

import           Control.Applicative (liftA2)
import           Control.Monad.Trans.Writer (WriterT, censor)

-- | Clear the log of a @WriterT@
-- 
clear :: (Monoid w, Monad m) => WriterT w m a -> WriterT w m a
clear = censor $ const mempty

-- | For monads, @ void_ = return () @.
-- 
void_ :: Applicative f => f ()
void_ = pure ()
