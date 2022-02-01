module Gadgets.Text.Lazy.Builder where

import           Control.Monad.Trans.State 
  (State, StateT, execState, execStateT, modify)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B

-- | A type class of converting a type to a Lazy "Builder" of "Text"s.
class Buildable a where
  build :: a -> B.Builder

instance Buildable Char where
  build = B.singleton
  {-# INLINE build #-}

instance Buildable a => Buildable [a] where
  build = mconcat . map build
  {-# INLINE build #-}

instance Buildable T.Text where
  build = B.fromText
  {-# INLINE build #-}

instance Buildable LT.Text where
  build = B.fromLazyText
  {-# INLINE build #-}

-- | Append a "Buildable" to the "Builder".
append :: Buildable a => Monad m => a -> StateT B.Builder m ()
append = modify . flip (<>) . build
{-# INLINE append #-}

-- | Build a Lazy "Text" in a monadic environment.
runBuilderT :: Monad m => StateT B.Builder m a -> m LT.Text
runBuilderT = fmap B.toLazyText . flip execStateT mempty
{-# INLINE runBuilderT #-}

-- | Build a Lazy "Text" in a pure environment.
runBuilder :: State B.Builder a -> LT.Text
runBuilder = B.toLazyText . flip execState mempty
{-# INLINE runBuilder #-}

-- | An example demonstrating the build process.
buildExample :: LT.Text
buildExample = runBuilder $ do
  append "String example"
  append ';'
  append ' '
  append (T.pack "Strict text example, ")
  append (LT.pack "Lazy text example, ")
  append ["Many\n", "String\n", "Example~"]
