import Prelude hiding (max)

e :: Double
e = exp 1

double :: Int -> Int
double x = x * 2

max :: Int -> Int -> Int
max x y = if x > y then x else y

max' :: Int -> Int -> Int
max' x y
  | x > y     = x
  | otherwise = y

fib :: Integer -> Integer
fib n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fib' :: Integer -> Integer
fib' n | n == 0 = 0
fib' n | n == 1 = 1
fib' n = fib' (n - 1) + fib' (n - 2)

fib'' :: Integer -> Integer
fib'' 0 = 0
fib'' 1 = 1
fib'' n = fib'' (n - 1) + fib'' (n - 2)

first :: (a,b) -> a
first (x,y) = x

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (x:xs) = False

err :: a
err = err

foo :: Num a => a
foo = foo + 1

len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs

collatz :: Integer -> Integer
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzSeq :: Integer -> [Integer]
collatzSeq 1 = [1]
collatzSeq n = n : collatzSeq (collatz n)

sumPairwise :: [Int] -> [Int]
sumPairwise []  = []
sumPairwise [x] = [x]
sumPairwise (x : y : zs) = x + y :sumPairwise
 zs










