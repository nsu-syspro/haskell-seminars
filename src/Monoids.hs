module Monoids where

import Prelude hiding
  (Semigroup, Semigroup(..), Monoid, Monoid(..))

import Data.List.NonEmpty

class Semigroup a where
  (<>) :: a -> a -> a

  sconcat :: NonEmpty a -> a
  sconcat (x :| xs) = foldr (<>) x xs

  stimes :: Integral b => b -> a -> a
  stimes n _ | n < 0 = error "less than zero"
  stimes 0 x = x
  stimes n x = x <> stimes (n - 1) x

  -- x ^ n = ...
  --
  -- x <> x <> x <> x <> x <> x
  --
  -- (x <>  x <> x)  <> (x <>  x <> x)
  -- (x <> (x <> x)) <> (x <> (x <> x))


class Semigroup a => Monoid a where
  mempty :: a

  mappend :: a -> a -> a
  mappend = (<>)

  mconcat :: [a] -> a
  mconcat = foldr (<>) mempty

newtype Sum a = Sum { getSum :: Num a => a }

instance Semigroup (Sum Int) where
  Sum x <> Sum y = Sum (x + y)

instance Monoid (Sum Int) where
  mempty = Sum 0


newtype Product a = Product { getProduct :: Num a => a }

instance Semigroup (Product Int) where
  Product x <> Product y = Product (x * y)

instance Monoid (Product Int) where
  mempty = Product 0


newtype Min a = Min { getMin :: Num a => a }

instance Semigroup (Min Int) where
  Min x <> Min y = Min (x `min` y)


instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []


instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a, b) <> (c, d) = (a <> c, b <> d)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)


-- Endomorphism
--
-- f <> g ~ f . g
--
-- Is it associative?
--
-- (f . g) . h = f . (g . h)
-- ((f . g) . h) x = (f . (g . h)) x
--
instance Semigroup (a -> a) where
  (<>) = (.)

instance Monoid (a -> a) where
  mempty = id



