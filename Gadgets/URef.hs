{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Gadgets.URef where

import           Control.Monad.ST (ST)
import           Data.Array.IO (IOUArray)
import           Data.Array.ST
    (MArray, STUArray, newArray, readArray, writeArray)
import           Control.Monad.ST.Unsafe (unsafeSTToIO)

-- | Type alias for the unboxed reference, which is implemented as an unboxed
-- @IOUArray@ (with a length of 1).
-- 
type IOURef e = IOUArray Int e

-- | Type alias for the unboxed reference, which is implemented as an unboxed
-- @STUArray@ (with a length of 1).
-- 
type STURef s e = STUArray s Int e

-- | Allocates a new reference to the given value.
-- 
newURef :: MArray a e m => e -> m (a Int e)
newURef = newArray (0, 0)

-- | Strictly modifies the value the reference refers to by the given function.
-- 
modifyURef :: MArray a e m => a Int e -> (e -> e) -> m ()
modifyURef ptr f = do
  v <- readURef ptr
  writeURef ptr $! f v

-- | Reads the value the reference refers to.
-- 
readURef :: MArray a e m => a Int e -> m e
readURef = flip readArray 0 

-- | Overwrites the value the reference refers to.
-- 
writeURef :: MArray a e m => a Int e -> e -> m ()
writeURef = flip writeArray 0
