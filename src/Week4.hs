{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE BangPatterns #-}

import Prelude hiding (product, reverse, (++))

-- fst :: (a, b) -> a

-- const
f1 :: a -> b -> a
f1 x y = x

if' :: Bool -> a -> a -> a
if' cond t f = if cond then t else f


-- flip
f2 :: (a -> b -> c) -> (b -> a -> c)
f2 f x y = f y x
--f2 f = \x y -> f y x

-- curry
f3 :: ((a, b) -> c) -> (a -> b -> c)
f3 f = \x y -> f (x, y)

-- uncurry
f4 :: (a -> b -> c) -> ((a, b) -> c)
f4 f = \(x, y) -> f x y


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (uncurry f) $ zip xs ys

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f = (map (uncurry f) .) . zip



-- ($) -- application
--f5 :: (a -> b) -> (a -> b)
--f5 = id
f5 :: (a -> b) -> a -> b
f5 f = f

-- (.) -- function composition
f6 :: (b -> c) -> (a -> b) -> (a -> c)
f6 f g = \x -> f (g x)

-- point-free style
--
-- point comes from Topoly theory
-- where "point" is a point in space (e.g. (x, y))
--
-- Point-free style is the on where you do not mention the "point"
--
sumEvenSquares :: [Int] -> Int
sumEvenSquares = sum . filter even . map (^2)

sumEvenSquares' :: [Int] -> Int
sumEvenSquares' =
  let
    square :: Int -> Int
    square x = x * x

    foo f x = let x' = g in x'
      where
        g = f x

  in sum . filter even . map square

sumEvenSquares'' :: [Int] -> Int
sumEvenSquares'' = sum . filter even . map square
 where
  square x = x * x

cmp :: Ordering -> Bool
cmp o = case o of
  EQ -> f
  LT -> False
  GT -> False
 where
  f = True

cmpKeys :: Ord k => (k, v) -> (k, v) -> Ordering
cmpKeys (k1, _) (k2, _) = compare k1 k2

cmpKeys' :: Ord k => (k, v) -> (k, v) -> Ordering
cmpKeys' = compare `on` fst

on :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
on cmp f x y = cmp (f x) (f y)

product :: Num a => [a] -> a
product xs = foldr (*) 1 xs

data LRList a = Nil | a :< LRList a | LRList a :> a
  deriving (Show)

-----------------
-- Day 2
-----------------

(++) :: [a] -> [a] -> [a]
[]       ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

--   reverse (0 : 1 : 2 : [])
-- ~ reverse (1 : 2 : []) ++ [0]
-- ~ (reverse (2 : []) ++ [1]) ++ [0]
-- ~ ((reverse [] ++ [2]) ++ [1]) ++ [0]
-- ~ (([] ++ [2]) ++ [1]) ++ [0]
-- ~  ([2] ++ [1]) ++ [0]
-- ~  ((2 : []) ++ [1]) ++ [0]
-- ~   (2 : ([] ++ [1])) ++ [0]

reverseAcc :: [a] -> [a]
reverseAcc = go []
 where
  go :: [a] -> [a] -> [a]
  go !acc []       = acc
  go !acc (x : xs) = go (x : acc) xs


sumSlow :: Num a => [a] -> a
sumSlow [] = 0
sumSlow (x : xs) = x + sumSlow xs

sumAcc :: Num a => [a] -> a
sumAcc = go 0
 where
  go acc [] = acc
  go acc (x : xs) = go (x + acc) xs


--   sumAcc (1 : 2 : 3 : [])
-- ~ go 0 (1 : 2 : 3 : [])
-- ~ go (0 + 1) (2 : 3 : [])
-- ~ go ((0 + 1) + 2) (3 : [])
-- ~ go (((0 + 1) + 2) + 3) []
-- ~ (((0 + 1) + 2) + 3)
-- ~ ((1 + 2) + 3)
-- ~ (3 + 3)
-- ~ 6


sumAcc' :: Num a => [a] -> a
sumAcc' = go 0
 where
  go !acc [] = acc
  go !acc (x : xs) = go (x + acc) xs

-- Bang pattern
--
-- Enabled by default since GHC2021
-- Before needs to be enabled with pragma
--
-- {-# LANGUAGE BangPatterns #-}
--
-- Forces not full evaluation, but only to WHNF
-- (Weak Head Normal Form)

sumAcc'' :: Num a => [a] -> a
sumAcc'' = go 0
 where
  go acc [] = acc
  go acc (x : xs) =
    let acc' = x + acc
    in  acc' `seq` go acc' xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f = go
 where
  go !acc [] = acc
  go !acc (x : xs) = go (f acc x) xs

sumAcc''' :: Num a => [a] -> a
sumAcc''' = foldl' (+) 0

-- Useful materials:
--
--   https://tech.fpcomplete.com/haskell/tutorial/all-about-strictness/
--   https://wiki.haskell.org/Foldr_Foldl_Foldl'
