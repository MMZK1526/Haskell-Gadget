{-# LANGUAGE PatternSynonyms #-}

module Gadgets.Set where

import           Data.Set (Set, empty)

pattern Empty :: Set a
pattern Empty <- empty
  where
    Empty = empty
