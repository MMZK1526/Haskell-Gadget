module Gadgets.Monad where

-- | @ void_ = return () @
-- 
void_ :: Monad m => m ()
void_ = return ()
