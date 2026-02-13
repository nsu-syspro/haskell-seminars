
import Prelude hiding (max)

--module Intro where

e :: Double
--e :: Floating a => a
e = exp 1

max :: Int -> Int -> Int
max a b = if a > b then a else b

max' :: Int -> Int -> Int
max' a b | a > b     = a
max' a b | otherwise = a

max'' :: Int -> Int -> Int
max'' a b | a > b = a
max'' a b         = a

fst' :: (a, b) -> a
fst' (a, b) = a

fib :: Integer -> Integer
fib 0 = 0
fib n | n == 1 = 1
fib n = fib (n - 1) + fib (n - 2)


collatz :: Integer -> Integer
collatz n
  | even n    = n `div` 2
  | otherwise = 3 * n + 1

-- >>> collatzSeq 3
-- [4,2,1]

collatzSeq :: Integer -> [Integer]
collatzSeq 1 = [1]
collatzSeq n = n : collatzSeq (collatz n)

collatzSeq' :: Integer -> [Integer]
collatzSeq' n
  | n == 1 = [1]
  | otherwise = n : collatzSeq' (collatz n)

sumPairwise :: [Integer] -> [Integer]
sumPairwise []  = []
sumPairwise [x] = [x]
--sumPairwise (x:[]) = [x]
sumPairwise (x : y : xs) = (x + y) : sumPairwise xs

-- (x : xs)
-- (y : ys)
