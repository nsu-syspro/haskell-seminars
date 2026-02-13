alph :: String
alph = 'a' : alph

foo :: [a] -> [[a]] -> [[a]]
foo = zipWith (:)
--foo x = zipWith (:) x
--foo x y = zipWith (:) x y

myZip :: [a] -> [b] -> [(a, b)]
myZip = zipWith (,)
--myZip = zipWith (\x y -> (x, y))


-- f :: a -> a
f :: a
f = f f

g :: a -> b
g = g g

u :: a
u = undefined

--   f1 f2 :: a -> b
--   f2 :: (a -> b)
--   f1 :: (? -> ?) -> (a -> b)
-- ~ f1 :: (a -> b) -> (a -> b)
--         |______|    |______|
--             a           b


-- Fixed-point combinator
--  or Y-combinator

y :: (a -> a) -> a
y g = g (y g)

alph' :: [Char]
alph' = y ('a' :)

-- g = ('a' :)
--
--   alph'
-- ~ y ('a' :)
-- ~ ('a' :) (y ('a' :))
-- ~ ('a' :) (('a' :) (y ('a' :)))
-- ~ 'a' : 'a' : y ('a' :)

--y' :: (a -> a) -> a
--y' = \f -> ((\x -> (f (x x))) (\x -> f (x x)))



