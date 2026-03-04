{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeClasses where

import Prelude hiding (Eq(..), elem)


-- Ad-hoc polymorphism
--   - overloading
--   - type classes

infix 4 ==
infix 4 /=

class Eq a where
  {-# MINIMAL (==) | (/=) #-}
  (==) :: a -> a -> Bool
  x == y = not (x /= y)
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)

instance Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False

-- Requires
-- {-# LANGUAGE MultiParamTypeClasses #-}
class B where
  myId :: a -> a

instance B where
  myId = id

foo :: a -> a
foo = myId

instance Eq a => Eq [a] where
  [] == []         = True
  [] == _          = False
  _  == []         = False
  (x:xs) == (y:ys) = x == y && xs == ys

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = x == y || x `elem` ys

instance Eq Int where
  x == y = case x - y of
    0 -> True
    _ -> False

newtype Mod3 = Mod3 { unMod3 :: Int }
  deriving Show

instance Eq Mod3 where
  Mod3 x == Mod3 y = (x `mod` 3) == (y `mod` 3)
  
onlyMod3 :: Int -> [Int] -> [Int]
onlyMod3 e = filter ((== Mod3 e) . Mod3)

-- unMod3 . (_) . Mod3

class ToInts a where
  toInts :: a -> [Int]

instance ToInts Int where
  --toInts x = [x]
  toInts = (: [])

instance ToInts Bool where
  toInts False = [0]
  toInts True  = [1]

instance {-# OVERLAPS #-} ToInts [Int] where
  toInts = id

instance (ToInts a, ToInts b) => ToInts (a, b) where
  toInts (x, y) = toInts x ++ toInts y

instance ToInts a => ToInts [a] where
  toInts = concatMap toInts

instance Enum a => ToInts a where
  toInts x = [fromEnum x]

-- Truly undecidable instance

data A a = A deriving Show

class F x where
  bar :: x -> x
  bar = id

--instance F (A a) => F (A a)

instance F (A (A a)) => F (A a)


data Foo x = Foo x
  deriving Show

newtype Bar x = Bar x
  deriving Show

test1 = case undefined of { _     -> 42 }
test2 = case undefined of { Foo _ -> 42 }
test3 = case undefined of { Bar _ -> 42 }

