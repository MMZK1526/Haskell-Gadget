{-# LANGUAGE TypeOperators #-}

module Gadgets.ND where

import           Control.Monad
import           Data.Function
import           Data.Functor.Identity
import           Data.Void

type TOP     = ()
type BOTTOM  = Void
type Not a   = a -> BOTTOM
type a && b  = (a, b)
type a || b  = Either a b
type a <-> b = (a -> b) && (b -> a)

type ND = Identity


--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

-- Natural Deduction in Haskell.
-- A propositional formula is valid iff the corresponding type signature can be
-- implemented with the rules below.

-- | A function that proves "a && b, a -> c, b -> d |- c && d".
example1 :: a && b -> (a -> c) -> (b -> d) -> ND (c && d)
example1 aAb a2c b2d = do
  (a, b) <- andE aAb
  c      <- ifE a2c a
  d      <- ifE b2d b
  cAd    <- andI c d
  tick cAd

-- | "a, c -> a |- c" is not valid, thus the following function cannot be
-- implemented with our rules (here we cheated with "undefined").
example2 :: a -> (c -> a) -> ND c
example2 a c2a = undefined


--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

tick :: a -> ND a
tick = pure

andI :: a -> b -> ND (a && b)
andI = (pure .) . (,)

andE :: a && b -> ND (a, b)
andE = pure

orI :: a -> ND (a || b, b || a)
orI = pure . liftM2 (,) Left Right

orE :: a || b -> (a -> ND c) -> (b -> ND c) -> ND c
orE aOb a2c b2c = case aOb of
  Left a  -> a2c a
  Right b -> b2c b

notI :: (a -> ND BOTTOM) -> ND (Not a)
notI = pure . (runIdentity .)

notE :: Not a -> a -> ND BOTTOM
notE = (pure .)

bottomI :: a -> Not a -> ND BOTTOM
bottomI = (pure .) . (&)

bottomE :: BOTTOM -> ND a
bottomE b = pure $ case b of

ifI :: (a -> ND b) -> ND (a -> b)
ifI = pure . (runIdentity .)

ifE :: (a -> b) -> a -> ND b
ifE = (pure .)

iffI :: (a -> ND b) -> (b -> ND a) -> ND (a <-> b)
iffI a2b b2a = pure (runIdentity . a2b, runIdentity . b2a)

iffE :: (a <-> b) -> ND (a -> b, b -> a)
iffE = pure

topI :: ND TOP
topI = pure ()

modusPollens :: (a -> ND b) -> Not b -> ND (Not a)
modusPollens = (pure .) . flip (.) . (runIdentity .)

notNotI :: a -> ND (Not (Not a))
notNotI = pure . (&)

-- The type system of Haskell corresponds to intuitionistic logic. In other
-- words, the Law of Excluded Middle (i.e. anything is either true or false)
-- does not come naturally. In order to use LEM or double negation elimination,
-- we have to assume one of them.

notNotE :: Not (Not a) -> ND a
notNotE nna = do
  aOna <- lem
  orE aOna tick $ \na -> do
    bottom <- bottomI na nna
    a      <- bottomE bottom
    tick a

lem :: ND (a || Not a)
lem = do
  nnLem <- notI $ \nLem -> do
    na  <- notI $ \a -> do
      (a2na, _) <- orI a
      bottomI a2na nLem
    nna <- notI $ \na -> do
      (_, a2na) <- orI na
      bottomI a2na nLem
    bottomI na nna
  notNotE nnLem


--------------------------------------------------------------------------------
-- 汉化
--------------------------------------------------------------------------------

我滴任务宛城啦 :: a -> ND a
我滴任务宛城啦 = tick

且介入 :: a -> b -> ND (a && b)
且介入 = andI

且去除 :: a && b -> ND (a, b)
且去除 = andE

或介入 :: a -> ND (a || b, b || a)
或介入 = orI

或去除 :: a || b -> (a -> ND c) -> (b -> ND c) -> ND c
或去除 = orE

否介入 :: (a -> ND BOTTOM) -> ND (Not a)
否介入 = notI

否去除 :: Not a -> a -> ND BOTTOM
否去除 = notE

假介入 :: a -> Not a -> ND BOTTOM
假介入 = bottomI

假去除 :: BOTTOM -> ND a
假去除 = bottomE

蕴含介入 :: (a -> ND b) -> ND (a -> b)
蕴含介入 = ifI

蕴含去除 :: (a -> b) -> a -> ND b
蕴含去除 = ifE

等价介入 :: (a -> ND b) -> (b -> ND a) -> ND (a <-> b)
等价介入 = iffI

等价去除 :: (a <-> b) -> ND (a -> b, b -> a)
等价去除 = iffE

真介入 :: ND TOP
真介入 = topI

反证法 :: (a -> ND b) -> Not b -> ND (Not a)
反证法 = modusPollens

双重否定介入 :: a -> ND (Not (Not a))
双重否定介入 = notNotI

双重否定去除 :: Not (Not a) -> ND a
双重否定去除 = notNotE

排中律 :: ND (a || Not a)
排中律 = lem
