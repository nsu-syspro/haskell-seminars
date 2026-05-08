import Data.Function (fix)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

--fact n = (\f -> n * f (n - 1)) fact

fact2 :: Int -> Int
fact2 n = if n == 0 then 1 else n * fact2 (n - 1)

fact3 :: Int -> Int
fact3 n = (\f -> if n == 0 then 1 else n * f (n - 1)) fact3

-- >>> fact 5
-- 120
-- >>> fact' 5
-- 120
-- >>> fact'' 5
-- 120

fact' :: Int -> Int
fact' = fix factHelper

factHelper :: (Int -> Int) -> (Int -> Int)
factHelper f 0 = 1
factHelper f n = n * f (n - 1)

fact'' :: Int -> Int
fact'' = fix (\f n -> if n == 0 then 1 else n * f (n - 1))

-- Does not have proper type:
--omega = (\x -> x x) (\x -> x x)
