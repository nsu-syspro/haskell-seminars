import Prelude hiding (Bool(..))

-- * Church encoding

-- * List encoding
--data List a = Cons a (List a) | Nil
data List a where
  Cons :: a -> List a -> List a
  Nil  :: List a

foldrList :: (a -> b -> b) -> b -> List a -> b
foldrList s z Nil = z
foldrList s z (Cons x xs) = s x (foldrList s z xs)

-- Isomorphism between List a and some function

-- Catamorphism: from List to function
foldList :: List a -> (forall b. (a -> b -> b) -> b -> b)
foldList xs s z = foldrList s z xs

-- Anamorphism: from function to List
unfoldList :: forall a . (forall b. (a -> b -> b) -> b -> b) -> List a
unfoldList f = f Cons Nil

--foldr0 :: [a] -> (a -> Int -> Int) -> Int -> Int
--foldr0 xs s z = foldr s z xs
--foo = unfoldList (foldr0 [1,2])

-- We expect:
--   foldList . unfoldList = id
--   unfoldList . foldList = id

-- * Bool encoding
--data Bool = True | False
data Bool where
  True :: Bool
  False :: Bool

foldBool :: Bool -> (b -> b -> b)
foldBool True t f = t
foldBool False t f = f
--foldBool b = \t f -> if b then t else f

unfoldBool :: (forall b. b -> b -> b) -> Bool
unfoldBool f = f True False

data Nat where
  Succ :: Nat -> Nat
  Zero :: Nat

foldNat :: Nat -> ((b -> b) -> b -> b)
foldNat (Succ x) s z = s (foldNat x s z)
foldNat Zero     s z = z

unfoldNat :: (forall b. (b -> b) -> b -> b) -> Nat
unfoldNat f = f Succ Zero

-- pred in Haskell
pred :: Nat -> Nat
pred (Succ n) = n
pred Zero = Zero

-- * Pair

data Pair a b where
  Pair :: a -> b -> Pair a b

foldPair :: Pair a b -> ((a -> b -> p) -> p)
foldPair (Pair x y) p = p x y

unfoldPair :: (forall p. (a -> b -> p) -> p) -> Pair a b
unfoldPair f = f Pair
