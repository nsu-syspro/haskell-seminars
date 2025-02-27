import Prelude hiding (product)

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



