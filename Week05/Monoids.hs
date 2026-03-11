module Monoids where

import Prelude hiding (Semigroup(..), Monoid(..))
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.Monoid as M
import Data.List (foldl', sortBy)
import Data.Ord (comparing)

class Semigroup a where
  {-# MINIMAL (<>) | sconcat #-}
  (<>) :: a -> a -> a
  x <> y = sconcat (x :| [y])

  -- x <> (y <> ...)
  sconcat :: NonEmpty a -> a
  sconcat (x :| xs) = foldr (<>) x xs

  -- > stimes 3 a
  -- a <> a <> a
  --
  -- Can be implemented in O(1) for idempotent semigroup
  -- (e.g. any semilattice)
  stimes :: Integral b => b -> a -> a
  stimes n _ | n <= 0 = error "..."
  stimes n x = x <> stimes (n - 1) x

class Semigroup a => Monoid a where
  {-# MINIMAL mempty | mconcat #-}
  mempty :: a
  mempty = mconcat []

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
  
sum' :: Num a => [a] -> a
sum' = getSum . mconcat . map Sum

sum'' :: Num a => [a] -> a
sum'' = M.getSum . M.mconcat . map M.Sum

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (x1, y1) <> (x2, y2) = (x1 <> x2, y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

average :: (Integral a, Fractional b) => [a] -> b
average [] = 0
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

average' :: (Integral a, Fractional b) => [a] -> b
average' [] = 0
average' xs =
  let (s, l) = foldl' (\(s', l') x -> (x + s', l' + 1)) (0, 0) xs
  in fromIntegral s / fromIntegral l

average'' :: (Integral a, Fractional b) => [a] -> b
average'' [] = 0
average'' xs =
  let (Sum s, Sum l) = mconcat $ map (\x -> (Sum x, Sum 1)) xs
  in fromIntegral s / fromIntegral l

average''' :: (Integral a, Fractional b) => [a] -> b
average''' [] = 0
average''' xs =
  let (M.Sum s, M.Sum l) = foldl' (M.<>) M.mempty $ map (\x -> (M.Sum x, M.Sum 1)) xs
  in fromIntegral s / fromIntegral l

-- More info about performance analysis of average:
--
--   https://book.realworldhaskell.org/read/profiling-and-optimization.html

newtype Product a = Product { getProduct :: a }
  deriving Show

instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product (x * y)

instance Num a => Monoid (Product a) where
  mempty = Product 1

instance Semigroup a => Semigroup (Maybe a) where
  Just x <> Just y = Just (x <> y)
  Nothing <> y = y
  x <> Nothing = x

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

-- Endomorphism
--
-- f,g,h :: a -> a
--
--   (f . g) . h = f . (g . h)
--
-- f <> g = f . g
--
-- id . f = f . id = f

newtype Endo a = Endo { appEndo :: a -> a }
  
instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

-- ghci> (drop 5 <> take 5) [1..10]
-- [6,7,8,9,10,1,2,3,4,5]
--
-- drop 5 :: [a] -> [a]
--   (drop 5 <> take 5) [1..10]
-- ~ drop 5 [1..10] ???? take 5 [1..10]
-- ~ drop 5 [1..10] <> take 5 [1..10]

instance Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a

instance Monoid b => Monoid (a -> b) where
  mempty :: a -> b
  mempty = const mempty

instance Semigroup Ordering where
  EQ <> x = x
  LT <> _ = LT
  GT <> _ = GT

instance Monoid Ordering where
  mempty = EQ

-- | Sort strings:
--   1. By length
--   2. By number vowels
--   3. Lexicographically
--
-- This is monoid:
--   a -> (a -> Ordering)
-- because this is also monoid
--   a -> Ordering
--
mySort :: [String] -> [String]
mySort = sortBy $
     comparing length
  <> comparing (length . filter isVowel)
  <> compare

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou") -- y -- not a vowel
