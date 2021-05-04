module Gadgets.Monad where

-- | For monads, @ void_ = return () @.
-- 
void_ :: Applicative f => f ()
void_ = pure ()
