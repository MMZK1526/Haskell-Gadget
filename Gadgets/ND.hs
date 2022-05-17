{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}

module ND where

import           Control.Monad
import           Data.Function
import           Data.Functor.Identity
import           Data.Void

type TOP     = ()
type BOTTOM  = Void
type Not a   = a -> BOTTOM
infixl 3 &&
type a && b  = (a, b)
infixl 2 ||
type a || b  = Either a b
infixr 1 <->
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

-- | A function that proves the equivalence of currying:
-- "(a && b -> c) <-> (a -> b -> c)".
柯里化等价 :: ND ((a && b -> c) <-> (a -> b -> c))
柯里化等价 = 等价介入 往 返
  where
    往 甲且乙蕴含丙 = 蕴含介入 $ \甲 -> 蕴含介入 $ \乙 -> do
      甲且乙 <- 且介入 甲 乙
      丙 <- 甲且乙蕴含丙 `蕴含去除` 甲且乙
      我滴任务宛城啦 丙
    返 甲蕴含乙蕴含丙 = 蕴含介入 $ \甲且乙 -> do
      (甲, 乙) <- 且去除 甲且乙
      乙蕴含丙 <- 甲蕴含乙蕴含丙 `蕴含去除` 甲
      丙 <- 乙蕴含丙 `蕴含去除` 乙
      我滴任务宛城啦 丙

-- | Pierce's Law: "((a -> b) -> a) -> a". This is equivalent to LEM.
pierceLaw :: ND (((a -> b) -> a) -> a)
pierceLaw = ifI $ \_a1 {- (a -> b) -> a -} -> do
  _p1 {- a || !a -} <- lem
  orE _p1 tick $ \_a2 {- !a -} -> do
    _p2 {- a -> b -} <- ifI $ \_a3 {- a -} -> do
      bottom <- bottomI _a3 _a2
      bottomE bottom
    ifE _a1 _p2

type DeMorgan1 a b = Not (a && b) <-> (Not a || Not b)
type DeMorgan2 a b = Not (a || b) <-> (Not a && Not b)
-- | De Morgan's Law: "(!(a && b) <-> !a || !b) && (!(a || b) <-> !a && !b)".
deMorganLaw :: ND (DeMorgan1 a b && DeMorgan2 a b)
deMorganLaw = join $ liftM2 andI (iffI dm1Fore dm1Back) (iffI dm2Fore dm2Back)
  where
    dm1Fore notOfaAb = do
      aOna <- lem
      orE aOna 
        (\a -> do
          nb         <- notI $ \b -> do
            aAb <- andI a b
            bottomI aAb notOfaAb
          (_, naOnb) <- orI nb
          tick naOnb)
        (\na -> do
          (naOnb, _) <- orI na
          tick naOnb)
    dm1Back naOnb    = orE naOnb 
      (\na -> notI (\aAb -> do
        (a, _) <- andE aAb
        bottomI a na))
      (\nb -> notI (\aAb -> do
        (_, b) <- andE aAb
        bottomI b nb))
    dm2Fore notOfaOb = do
      na <- notI $ \a -> do
        (aOb, _) <- orI a
        bottomI aOb notOfaOb
      nb <- notI $ \b -> do
        (_, aOb) <- orI b
        bottomI aOb notOfaOb
      andI na nb
    dm2Back naAnb    = do
      (na, nb) <- andE naAnb
      notI $ \aOb -> orE aOb (notE na) (notE nb)


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
