{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Gadgets.ZipList () where

import qualified Control.Applicative as A (ZipList(..))
import           GHC.Exts (IsList(..))

type ZipList = A.ZipList

getZipList :: ZipList a -> [a]
getZipList = A.getZipList

instance IsList (ZipList a) where
  type Item (ZipList a) = a
  fromList              = A.ZipList
  toList                = A.getZipList
