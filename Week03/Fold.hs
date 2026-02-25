{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE BangPatterns #-}

module Fold where

import Prelude hiding (reverse, (++))
import Data.List (foldl')

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

--   reverse (1 : [2,3])
-- ~ reverse ([2,3]) ++ [1]
-- ~ reverse (2 : [3]) ++ [1]
-- ~ (reverse ([3]) ++ [2]) ++ [1]
-- ~ (reverse (3 : []) ++ [2]) ++ [1]
-- ~ ((reverse ([]) ++ [3]) ++ [2]) ++ [1]
-- ~ (([] ++ [3]) ++ [2]) ++ [1]
--
-- 1 + 2 + ... + n - 1 ~ O(n^2)
--

reverseAcc :: [a] -> [a]
reverseAcc = go []
 where
  go acc []     = acc
  go acc (x:xs) = go (x : acc) xs

reverseFold :: [a] -> [a]
reverseFold = foldl' (flip (:)) []

--   reverseAcc [1,2,3]
-- ~ go [] [1,2,3]
-- ~ go [] (1 : [2,3])
-- ~ go (1 : []) [2,3]
-- ~ go (1 : []) (2 : [3])
-- ~ go (2 : (1 : [])) ([3])
-- ~ go (3 : 2 : 1 : []) []
-- ~ 3 : 2 : 1 : []
-- ~ [3,2,1]
--

sumSlow :: [Int] -> Int
sumSlow []     = 0
sumSlow (x:xs) = x + sumSlow xs

sumFast :: [Int] -> Int
sumFast = go 0
 where
  go acc [] = acc
  go acc (x:xs) = go (x + acc) xs

sumFold :: [Int] -> Int
sumFold = foldr (+) 0

--   sumFast [1,2,3]
-- ~ go 0 [1,2,3]
-- ~ go 0 (1 : [2,3])
-- ~ go (1 + 0) [2,3]
-- ~ go (1 + 0) (2 : [3])
-- ~ go (2 + (1 + 0)) [3]
-- ~ go (2 + (1 + 0)) (3 : [])
-- ~ go (3 + (2 + (1 + 0))) []
-- ~ (3 + (2 + (1 + 0)))
-- ~ 3 + (2 + 1)
-- ~ 3 + 3
-- ~ 6

sumFast' :: [Int] -> Int
sumFast' = go 0
 where
  go !acc [] = acc
  go !acc (x:xs) = go (x + acc) xs

sumFold' :: [Int] -> Int
sumFold' = foldl' (+) 0

-- Bang pattern
--
-- Enabled by default since GHC2021
-- Before needs to be enabled with pragma
--
-- {-# LANGUAGE BangPatterns #-}
--
-- Forces not full evaluation, but only to WHNF
-- (Weak Head Normal Form)

sumFast'' :: [Int] -> Int
sumFast'' = go 0
 where
  go acc [] = acc
  go acc (x:xs) =
    let acc' = x + acc
    in acc' `seq` go acc' xs

data StrictPair a b = StrictPair !a !b
  deriving Show

strictFst :: StrictPair a b -> a
strictFst (StrictPair a _) = a

-- Materials
--
--   https://academy.fpblock.com/haskell/tutorial/all-about-strictness/
--   https://wiki.haskell.org/Foldr_Foldl_Foldl'
