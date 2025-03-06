{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (Eq, Eq(..), elem)

f :: a -> a
f x = x

f' :: a -> a
f' = id


p1, p2, p3, p4 :: (a, a) -> (a, a)
p1 (x,y) = (x,y)
p2 (x,y) = (y,x)
p3 (x,_) = (x,x)
p4 (_,y) = (y,y)

-- Ad-hoc polymorphism

class Eq a where
  {-# MINIMAL ((==) | (/=)) #-}
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)

infix 4 ==
infix 4 /=

instance Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False

instance (Eq a, Eq b) => Eq (a, b) where
  (x1, y1) == (x2, y2) = x1 == x2 && y1 == y2

instance Eq Int where
  0 == 0 = True
  0 == _ = False
  _ == 0 = False
  x == y = (x - y) == 0

instance Eq a => Eq [a] where
  (==) :: Eq a => [a] -> [a] -> Bool
  [] == [] = True
  [] == _  = False
  _  == [] = False
  (x : xs) == (y : ys) = x == y && (xs == ys)
  --_ == _ = False


elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x : xs) = x == a || elem a xs


newtype Mod3 = Mod3 { unMod3 :: Int }
  deriving (Show)

instance Eq Mod3 where
  (Mod3 x) == (Mod3 y) = (x `mod` 3) == (y `mod` 3)

onlyMod3 :: Int -> [Int] -> [Int]
onlyMod3 e = filter ((== Mod3 e) . Mod3)

-- unMod3 . (...) . Mod3

-- Requires {-# LANGUAGE DataKinds #-}
-- newtype Mod Nat = Mod3 n

class ToInts a where
  toInts :: a -> [Int]

instance ToInts Int where
  toInts x = [x]

instance ToInts [Int] where
  toInts = id

instance (ToInts a, ToInts b) => ToInts (a, b) where
  toInts (x, y) = toInts x ++ toInts y

instance ToInts a => ToInts [a] where
  toInts = concatMap toInts

instance Enum a => ToInts a where
  toInts x = [fromEnum x]

-- Example of truly undecidable instance

data A a = A deriving Show

class F x where
  foo :: x -> x
  foo = id

--instance F (A a) => F (A a)

instance F (A (A a)) => F (A a)
