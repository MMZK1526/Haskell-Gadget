{-# LANGUAGE TypeFamilies #-}

module Gadgets.ZipList where

import           Control.Applicative (ZipList(..))
import           GHC.Exts (IsList(..))

instance IsList (ZipList a) where
  type Item (ZipList a) = a
  fromList              = ZipList
  toList                = getZipList
