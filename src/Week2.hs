
import Prelude hiding (Bool (..), (||))

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

data Bool = False | True
  deriving (Show)

(||) :: Bool -> Bool -> Bool
True  || _ = True
False || y = y

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


