
import Prelude hiding (Bool(..), foldr)
import Data.Function (fix)

--data List a = Cons a (List a) | Nil
data List a where
  Cons :: a -> List a -> List a
  Nil :: List a

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f s [] = s
foldr f s (x : xs) = f x (foldr f s xs)

foldList :: [a] -> (forall b. (a -> b -> b) -> b -> b)
foldList xs f s = foldr f s xs

-- Claim: [a] is isomorphic to ((a -> b -> b) -> b -> b)

unfoldList :: (forall b. (a -> b -> b) -> b -> b) -> [a]
unfoldList f = f (:) []

-- * Church numerals

--data Bool = True | False
data Bool where
  True :: Bool
  False :: Bool

foldBool :: Bool -> (b -> b -> b)
foldBool True = \t f -> t
foldBool False = \t f -> f

unfoldBool :: (forall b. b -> b -> b) -> Bool
unfoldBool f = f True False

--data Nat = Succ Nat | Zero
data Nat where
  Succ :: Nat -> Nat
  Zero :: Nat

foldNat :: Nat -> ((b -> b) -> b -> b)
foldNat Zero = \s z -> z
foldNat (Succ n) = \s z -> s (foldNat n s z)

unfoldNat :: (forall b. (b -> b) -> b -> b) -> Nat
unfoldNat f = f Succ Zero

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n


--data Pair a b = Pair a b
data Pair a b where
  Pair :: a -> b -> Pair a b

foldPair :: Pair a b -> ((a -> b -> p) -> p)
foldPair (Pair a b) = \p -> p a b

unfoldPair :: (forall p. (a -> b -> p) -> p) -> Pair a b
unfoldPair f = f Pair

first :: Pair a b -> a
first (Pair a b) = a


-- * Fixed-point conversion

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fact' :: Int -> Int
fact' 0 = 1
fact' n = (\f -> n * f (n - 1)) fact'

factHelper :: (Int -> Int) -> Int -> Int
factHelper f 0 = 1
factHelper f n = n * f (n - 1)

fact'' :: Int -> Int
fact'' = fix factHelper

-- * Combinatory logic

-- Basis
--
-- S f g x = (f x) (g x)
-- K x y = x
--
-- Example:
--
-- I = S K K

