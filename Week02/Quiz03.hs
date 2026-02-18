import Prelude hiding (zipWith, tail)

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _  []     _     = []
zipWith _  _      []    = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys


-- Laziness
--
--   fib
-- ~ 0 : 1 : zipWith (+) fib (tail fib)
-- ~ 0 : 1 : zipWith (+) (0 : 1 : zw...) (tail fib)
-- ~ 0 : 1 : zipWith (+) (0 : 1 : zw...) (tail (0 : 1 : zw ...))
-- ~ 0 : 1 : zipWith (+) (0 : 1 : zw...) (1 : zw ...)
-- ~ 0 : 1 : (+) 0 1 : zipWith (+) (1 : zw...) (zw...)
-- ~ 


tail :: [a] -> [a]
tail [] = error "..."
tail (x:xs) = xs

foo :: [Integer]
foo = map (*4) [1..]


f1 :: a -> a
f1 x = x

f2 :: a -> b
f2 x = undefined

-- f2 :: Int -> Bool
-- f2 :: [Int] -> Int
-- ...

badMap :: (a -> b) -> [a] -> [b]
badMap f [] = []
badMap f (x:xs) = f x : f x : badMap f xs
