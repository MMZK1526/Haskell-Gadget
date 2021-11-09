{-# LANGUAGE OverloadedLists #-}

module Gadgets.RAList (
  RAList(Empty, (:<)), empty, fromList, head, singleton, tail, 
  toList, (!), (!?)
) where

import           Gadgets.RAList.Internal 
  (RAList(..), empty, fromList, head, singleton, tail, toList, (!), (!?))
import           Gadgets.RAList.IsList ()
import           Prelude hiding (head, tail)
