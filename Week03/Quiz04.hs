
-- const
f1 :: a -> b -> a
f1 a b = a

-- flip
f2 :: (a -> b -> c) -> b -> a -> c
f2 g b a = g a b

f2' :: (a -> b -> c) -> (b -> a -> c)
f2' g = \b a -> g a b

-- curry
f3 :: ((a, b) -> c) -> (a -> b -> c)
f3 g a b = g (a, b)

-- uncurry
f4 :: (a -> b -> c) -> ((a, b) -> c)
f4 g (a, b) = g a b

zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith (,)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (uncurry f) (zip xs ys)

-- ($)
f5 :: (a -> b) -> (a -> b)
f5 = id

f5' :: (a -> b) -> a -> b
f5' f a = f a

-- (.)
f6 :: (b -> c) -> (a -> b) -> (a -> c)
f6 f g = \x -> f (g x)

f6' :: (b -> c) -> (a -> b) -> (a -> c)
f6' f g x = f (g x)

-- f6  h t
-- f6' h t
--
-- ~ \x -> f6' h t x

-- Point-free style
--
-- "point" comes from Topology theory
-- where "point" means a point in space (e.g. (x,y))
--
-- Point-free style is the one where you do not mention the "point"

-- point-free
sumEvenSquares :: [Int] -> Int
sumEvenSquares = sum . filter even . map (^2)

sumEvenSquares' :: [Int] -> Int
sumEvenSquares' xs = (sum . filter even . map (^2)) xs

-- non-point-free
sumEvenSquares'' :: [Int] -> Int
sumEvenSquares'' xs = sum (filter even (map (^2) xs))

badFoldr :: (a -> b -> b) -> b -> [a] -> b
badFoldr f = foldl (flip f)

f8 :: (a -> b -> b) -> b -> [a] -> b
f8 = foldr

f8' :: (a -> b -> b) -> b -> [a] -> b
f8' _  = const

f8'' :: (a -> b -> b) -> b -> [a] -> b
f8'' _ b _ = b

data LRList a = Nil | a :< LRList a | LRList a :> a
  deriving Show

-- ghci> foldl (:>) Nil [1..10]
-- (((((((((Nil :> 1) :> 2) :> 3) :> 4) :> 5) :> 6) :> 7) :> 8) :> 9) :> 10
-- ghci> foldr (:<) Nil [1..10]
-- 1 :< (2 :< (3 :< (4 :< (5 :< (6 :< (7 :< (8 :< (9 :< (10 :< Nil)))))))))
-- ghci> badFoldr (:<) Nil [1..10]
-- 10 :< (9 :< (8 :< (7 :< (6 :< (5 :< (4 :< (3 :< (2 :< (1 :< Nil)))))))))

takeLR :: Int -> LRList a -> LRList a
takeLR 0 _  = Nil
takeLR n Nil = Nil
takeLR n (x :< xs) = x :< takeLR (n - 1) xs
takeLR n (xs :> x) = takeLR (n - 1) xs :> x
