
import Prelude hiding (zipWith, zip, elem)

alph :: [Char]
alph = 'a' : alph

-- (:) :: a -> [a] -> [a]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

foo :: [a] -> [[a]] -> [[a]]
foo = zipWith (:)

f :: a -> a
f = f f

y :: (a -> a) -> a
y g = g (y g)

-- Operator section
--
--   \x -> x + y
-- ~ (+ y)
--
--   \y -> x + y
-- ~ (x +)

alph' :: [Char]
alph' = y ('a' :)

a :: Integer
a = 42

-- Holes:
--bar = _ + 1











-- Defining data types

-- Synonyms

type IntChar = (Int, Char)

--type Pair a b = (a, b)

type Student = String
type Teacher = String

type StudentId = Integer

studentById :: StudentId -> Student
studentById idx = "Student " ++ show idx

teacherById :: Integer -> Teacher
teacherById idx = "Student"

-- Algebraic Data Types (ADTs)

data Color = Red | White | Blue | Green
  deriving (Show, Enum)

toWhite :: Color -> Color
toWhite _ = White


isWhite :: Color -> Bool
isWhite White = True
isWhite _     = False

--isWhite' :: Color -> Bool
--isWhite' c = c == White

--data Bool = False | True
--  deriving (Show, Eq)
--
--(||) :: Bool -> Bool -> Bool
--True  || _ = True
--False || y = y
--
--(&&) :: Bool -> Bool -> Bool
--True  && True = True
--_     && _    = False

infixr 2 ||


data SafeDivResult = DivByZero | Result Integer
 deriving Show

safeDiv :: Integer -> Integer -> SafeDivResult
safeDiv x y
  | y == 0 = DivByZero
  | otherwise = Result (x `div` y)

resultToInteger :: SafeDivResult -> Integer
resultToInteger DivByZero = 0
resultToInteger (Result x) = x


-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

-- Partial functions (avoid at all costs)
-- div, head, tail, last, init
-- fromJust, fromLeft

-- fromJust :: Maybe a -> a
-- fromJust (Just x) = x
-- fromJust Nothing = error "..."

data List a = Nil | Cons a (List a)

-- data [] a = [] | a : [a]

data Pair a b = Pair a b

concatMap' f x = concat (map f x)


fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

zip = zipWith (,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

--   0 : .... <- as
-- ~ 0 : 1 : ..... <- bs
-- ~ 0 : 1 : zipWith (+) fib (tail fib)
-- ~ 0 : 1 : zipWith (+) (0 : as) (1 : bs) 
-- ~ 0 : 1 : (0 + 1) : zipWith (+) (as) (bs)
-- ~ 0 : 1 : (0 + 1) : zipWith (+) (as) (bs)
--
--

data Point a = Point
   { pointX :: a
   , pointY :: a
   }
 deriving Show

sumCoords :: Point Int -> Int
sumCoords (Point {pointY = y}) = y + 1

shiftX :: Int -> Point Int -> Point Int
shiftX sh p = p { pointX = pointX p + sh }

--type PolarPoint = Point Double
--type CartPoint  = Point Double

newtype PolarPoint = PolarPoint (Point Double)
newtype CartPoint  = CartPoint  (Point Double)

bad :: PolarPoint -> CartPoint
bad (PolarPoint (Point x y)) = CartPoint (Point x y)

newtype Natural = Natural Integer
 deriving Show

constrNatural :: Integer -> Natural
constrNatural x
  | x < 0 = undefined
  | otherwise = Natural x

constrNaturalMay :: Integer -> Maybe Natural
constrNaturalMay x
  | x < 0 = Nothing
  | otherwise = Just (Natural x)


data NonEmptyList a = Single a | NonEmptyCons a (NonEmptyList a)

data Tree a = Leaf a | Branch (Tree a) (Tree a)
 deriving Show

height :: Tree a -> Int
height (Leaf _) = 0
height (Branch l r) = 1 + (height l `max` height r)

isomorphic :: Tree a -> Tree b -> Bool
isomorphic (Leaf _) (Leaf _) = True
isomorphic (Branch al ar) (Branch bl br) =
  isomorphic al bl && isomorphic ar br
isomorphic _ _ = False

--pointX :: Point a -> a
--pointX (Point x _) = x

-- [ a
-- , b
-- , c
-- ]

elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem v (x : xs) = v == x || v `elem` xs

