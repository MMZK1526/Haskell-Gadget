{-# LANGUAGE OverloadedLists #-}

module Gadgets.RAList (
  RAList(Empty, (:<)), empty, fromList, head, modify, singleton, tail, toList,
  update, update', (!), (!?)
) where

import           Gadgets.RAList.Internal 
  (RAList(..), empty, fromList, head, modify, singleton, tail, toList, update, 
  update', (!), (!?))
import           Gadgets.RAList.IsList ()
import           Prelude hiding (head, tail)
