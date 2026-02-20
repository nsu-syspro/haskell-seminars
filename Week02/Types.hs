module Types where

-- Synonyms 
--
-- Denote *the same* type.
-- No type safety!
--
-- Used for saving characters on very long type signatures
-- and for documentation.

type IntPair = (Int, Int)

type StudentId = Int
type TeacherId = Int

t1 :: TeacherId
t1 = 24

inc :: StudentId -> StudentId
inc x = x + 1

-- Both allowed:

f1 :: TeacherId
f1 = inc t1

f2 :: StudentId
f2 = inc t1

-- Algebraic Data Type (ADT)

data Color = Red | Green | Blue
  deriving (Show, Eq)

data Color' where
  Red'   :: Color'
  Green' :: Color'
  Blue'  :: Color'
  deriving (Show)

isRed :: Color -> Bool
isRed Red = True
isRed _   = False

isGreen :: Color -> Bool
isGreen x = x == Green


data SafeDivResult = DivByZero | Result Integer
  deriving (Show)

safeDiv :: Integer -> Integer -> SafeDivResult
safeDiv _ 0 = DivByZero
safeDiv x y = Result (x `div` y)

-- Task: define following function
--
-- >>> safeDivInteger 5 0 
-- 0
-- >>> safeDivInteger 5 2 
-- 2
safeDivInteger :: Integer -> Integer -> Integer
safeDivInteger x y = resultToInteger (safeDiv x y)

-- 1. Define helper function
safeDivInteger' :: Integer -> Integer -> Integer
safeDivInteger' x y = resultToInteger (safeDiv x y)

resultToInteger :: SafeDivResult -> Integer
resultToInteger DivByZero  = 0
resultToInteger (Result x) = x

-- 2. Move helper function to inner with where clause
safeDivInteger'' :: Integer -> Integer -> Integer
safeDivInteger'' x y = resultToInteger (safeDiv x y)
  where
    resultToInteger :: SafeDivResult -> Integer
    resultToInteger DivByZero  = 0
    resultToInteger (Result x) = x

-- 3. Move helper function to inner with let ... in ... clause
safeDivInteger''' :: Integer -> Integer -> Integer
safeDivInteger''' x y =
  let resultToInteger :: SafeDivResult -> Integer
      resultToInteger DivByZero  = 0
      resultToInteger (Result x) = x
  in  resultToInteger (safeDiv x y)

-- The only difference between where and let ... in ...
-- relates to guards
--
-- Definitions in where can be used within guards

foo :: Int -> Int
foo n
  | positive n = 1
  | n < 0      = -1
 where
  positive n = n > 0

-- 4. Use case construct
safeDivInteger'''' :: Integer -> Integer -> Integer
safeDivInteger'''' x y =
  case safeDiv x y of
    DivByZero -> 0
    Result x  -> x

-- All pattern matching on functions can be replaced with case
safeDiv' :: Integer -> Integer -> SafeDivResult
safeDiv' x y = case (x, y) of
  (_, 0) -> DivByZero
  (x, y) -> Result (x `div` y)

safeDiv'' :: Integer -> Integer -> Maybe Integer
safeDiv'' _ 0 = Nothing
safeDiv'' x y = Just (x `div` y)

-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

-- Partial functions (avoid at all costs):
--   div, head, tail, last, init
--   fromJust, fromLeft ...

fromJust :: Maybe a -> a
fromJust Nothing  = error "..."
fromJust (Just x) = x

data List a = Empty | Cons a (List a)
-- data [] a = [] | a : [a]

data List' a where
  Empty' :: List' a
  Cons'  :: a -> List' a -> List' a

data Pair a b = Pair a b


-- Avoiding last, init

-- >>> extractLast [1,2,3]
-- Just ([1,2],3)
-- >>> extractLast' [1,2,3]
-- Just ([1,2],3)
--
extractLast :: [a] -> Maybe ([a], a)
extractLast xs = case reverse xs of
  []     -> Nothing
  (x:xs) -> Just (reverse xs, x)

extractLast' :: [a] -> Maybe ([a], a)
extractLast' [] = Nothing
extractLast' xs = Just (init xs, last xs)

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Show)

-- >>> height (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3))
-- 2
height :: Tree a -> Int
height (Leaf _)     = 0
height (Branch l r) = 1 + max (height l) (height r)

isomorphic :: Tree a -> Tree a -> Bool
isomorphic (Leaf _) (Leaf _) = True
isomorphic (Branch l1 r1) (Branch l2 r2) =
  isomorphic l1 l2 && isomorphic r1 r2


