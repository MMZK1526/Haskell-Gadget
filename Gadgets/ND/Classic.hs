{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Gadgets.ND.Classic where

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

-- | To support classical logic with LEM, we need to add some restriction to
-- the type variables we are using. A "PPT" either has a default value or a
-- function from it to any other type.
--
-- We could see that any type can implement "PPT" since it is either inhabitable
-- (where we can choose any value as the default) or isomorphic to "Void" (thus
-- having an equivalent to "absurd").
class PPT p where
  rep :: Either (p -> x) p

instance PPT TOP where
  rep = Right ()

instance PPT BOTTOM where
  rep = Left absurd

instance forall a b. (PPT a, PPT b) => PPT (a && b) where
  rep = case (rep :: (Either (a -> x) a), rep :: (Either (b -> x) b)) of
    (Left f, _)        -> Left $ \(a, _) -> f a
    (_, Left f)        -> Left $ \(_, b) -> f b
    (Right a, Right b) -> Right (a, b)

instance forall a b. (PPT a, PPT b) => PPT (a || b) where
  rep = case (rep :: (Either (a -> x) a), rep :: (Either (b -> x) b)) of
    (Right a, _)     -> Right $ Left a
    (_, Right b)     -> Right $ Right b
    (Left f, Left g) -> Left $ \case
      Left a  -> f a
      Right b -> g b

instance forall a b. (PPT a, PPT b) => PPT (a -> b) where
  rep = case (rep :: (Either (a -> x) a), rep :: (Either (b -> x) b)) of
    (_, Right b)      -> Right (const b)
    (Left f, _)       -> Right f
    (Right a, Left f) -> Left $ \a2b -> f $ a2b a


--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

-- Natural Deduction in Haskell.
-- A propositional formula is valid iff the corresponding type signature can be
-- implemented with the rules below.

-- | A function that proves "a && b, a -> c, b -> d |- c && d".
example1 :: (PPT a, PPT b, PPT c, PPT d)
         => a && b -> (a -> c) -> (b -> d) -> ND (c && d)
example1 aAb a2c b2d = do
  (a, b) <- andE aAb
  c      <- ifE a2c a
  d      <- ifE b2d b
  cAd    <- andI c d
  tick cAd

-- | "a, c -> a |- c" is not valid, thus the following function cannot be
-- implemented with our rules (here we cheated with "undefined").
example2 :: (PPT a, PPT c) => a -> (c -> a) -> ND c
example2 a c2a = undefined


--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

tick :: PPT a => a -> ND a
tick = pure

andI :: (PPT a, PPT b) => a -> b -> ND (a && b)
andI = (pure .) . (,)

andE :: (PPT a, PPT b) => a && b -> ND (a, b)
andE = pure

orI :: (PPT a, PPT b) => a -> ND (a || b, b || a)
orI = pure . liftM2 (,) Left Right

orE :: (PPT a, PPT b) => a || b -> (a -> ND c) -> (b -> ND c) -> ND c
orE aOb a2c b2c = case aOb of
  Left a  -> a2c a
  Right b -> b2c b

notI :: PPT a => (a -> ND BOTTOM) -> ND (Not a)
notI = pure . (runIdentity .)

notE :: PPT a => Not a -> a -> ND BOTTOM
notE = (pure .)

bottomI :: PPT a => a -> Not a -> ND BOTTOM
bottomI = (pure .) . (&)

bottomE :: PPT a => BOTTOM -> ND a
bottomE b = pure $ case b of

ifI :: (PPT a, PPT b) => (a -> ND b) -> ND (a -> b)
ifI = pure . (runIdentity .)

ifE :: (PPT a, PPT b) => (a -> b) -> a -> ND b
ifE = (pure .)

iffI :: (PPT a, PPT b) => (a -> ND b) -> (b -> ND a) -> ND (a <-> b)
iffI a2b b2a = pure (runIdentity . a2b, runIdentity . b2a)

iffE :: (PPT a, PPT b) => (a <-> b) -> ND (a -> b, b -> a)
iffE = pure

topI :: ND TOP
topI = pure ()

modusPollens :: (PPT a, PPT b) => (a -> ND b) -> Not b -> ND (Not a)
modusPollens = (pure .) . flip (.) . (runIdentity .)

notNotI :: PPT a => a -> ND (Not (Not a))
notNotI = pure . (&)

notNotE :: forall a. PPT a => Not (Not a) -> ND a
notNotE nna = case rep :: Either (a -> x) a of
  Right a -> pure a
  Left f  -> case nna f of

lem :: PPT a => ND (a || Not a)
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
