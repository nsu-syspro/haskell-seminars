module Monoids where

import Prelude hiding
  (Semigroup, Semigroup(..), Monoid, Monoid(..))

import Data.List.NonEmpty (NonEmpty, NonEmpty(..))
import Data.List (sortBy)
import Data.Ord (comparing)

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

newtype Sum a = Sum { getSum :: a }
  deriving Show

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0


-- >>> sum [0..10]
-- 55
-- >>> getSum $ mconcat $ map Sum [0..10]
-- 55


average :: (Integral a, Fractional b) => [a] -> b
average [] = 0
average xs =
  fromIntegral (sum xs) / fromIntegral (length xs)

average' :: (Integral a, Fractional b) => [a] -> b
average' xs = fromIntegral s / fromIntegral n
 where
  (s,n) = foldr f (0,0) xs
  f x (s,n) = (s + x, n + 1)

average'' :: (Integral a, Fractional b) => [a] -> b
average'' xs = fromIntegral s / fromIntegral n
 where
  (Sum s, Sum n) = mconcat $ map (\x -> (Sum x, Sum 1)) xs

newtype Product a = Product { getProduct :: a }

instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product (x * y)

instance Num a => Monoid (Product a) where
  mempty = Product 0


newtype Min a = Min { getMin :: a }

instance Ord a => Semigroup (Min a) where
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
-- (f . g) x = f (g x)
--
-- Is it associative?
--
-- (f . g) . h = f . (g . h)
--
--   ((f . g) . h) x
-- ~ (f . g) (h x)
-- ~ f (g (h x))
--
--   (f . (g . h)) x
-- ~ f ((g . h) x)
-- ~ f (g (h x))
--

newtype Endo a = Endo { appEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

instance Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a

instance Monoid b => Monoid (a -> b) where
  mempty = const mempty

-- >>> (drop 5 <> take 5) [0..10]
-- [5,6,7,8,9,10,0,1,2,3,4]

instance Semigroup Ordering where
  EQ <> x  = x
  LT <> _  = LT
  GT <> _  = GT

instance Monoid Ordering where
  mempty = EQ


-- | Sorts string:
--   1. By length
--   2. By number vowels
--   3. Lexicographically
--
-- This is also Monoid (a -> b):
--   a -> a -> Ordering 
-- ~ a -> (a -> Ordering)
--
-- This is also Monoid (a -> b):
--   a -> Ordering
mySort :: [String] -> [String]
mySort = sortBy $
     comparing length
  <> comparing (length . filter isVowel)
  <> compare

mySort' :: [String] -> [String]
mySort' = sortBy $ mconcat
  [ comparing length
  , comparing (length . filter isVowel)
  , compare
  ]

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

type Tree a = Tree' (Sum Int) a

data Tree' m a = Leaf a | Node m (Tree' m a) (Tree' m a)
  deriving Show

elemT :: Int -> Tree a -> a
elemT 0 (Leaf v) = v
elemT _ (Leaf _) = error "out of bounds"
elemT i (Node (Sum ln) l r)
  | i < ln = elemT i l
  | otherwise = elemT (i - ln) r

leaf :: a -> Tree a
leaf = Leaf

node :: Tree a -> Tree a -> Tree a
node l@(Leaf _)                r = Node (Sum 1) l r
node l@(Node n _ (Leaf _))     r = Node (n <> Sum 1) l r
node l@(Node n _ (Node m _ _)) r = Node (n <> m) l r

exampleTree :: Tree Char
exampleTree =
  node (node (node (leaf 'h') (leaf 'e')) (leaf 'l'))
         (node (leaf 'l') (leaf 'o'))
