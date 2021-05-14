{-# Language PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Gadgets.ZipList where

import qualified Control.Applicative as A (ZipList(..))
import           GHC.Exts (IsList(..))

type ZipList = A.ZipList

pattern ZipList :: [a] -> ZipList a
pattern ZipList a = A.ZipList a

getZipList :: ZipList a -> [a]
getZipList = A.getZipList

instance IsList (ZipList a) where
  type Item (ZipList a) = a
  fromList              = A.ZipList
  toList                = getZipList
