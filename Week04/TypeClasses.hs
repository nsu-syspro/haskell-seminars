{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
class A where
  myId :: a -> a

instance A where
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

