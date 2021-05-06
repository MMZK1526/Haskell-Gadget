{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Gadgets.Pure where

import           Control.Applicative (liftA2)
import           Control.Monad (ap, filterM, join)

import qualified Data.Text as T

data Misete = forall a. Show a => Misete a

type Text = T.Text

instance Show Misete where
  show (Misete a) = show a

instance {-# OVERLAPPABLE #-} (Num a, Applicative m) => Num (m a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

instance {-# OVERLAPPABLE #-} (Fractional a, Applicative m)
  => Fractional (m a) where
    (/)          = liftA2 (/)
    fromRational = pure . fromRational

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

ftext :: (Text -> Text) -> String -> String
ftext f s = T.unpack $ f $ T.pack s

powerSet :: [a] -> [[a]]
powerSet = filterM $ const [True, False]

primes :: [Integer]
primes = 2 : filter
  (ap (all . ((0 /=) .) . mod)
      (flip takeWhile primes . (. join (*)) . flip (<=))) [3, 5..]
