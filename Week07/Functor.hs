import Prelude hiding (Functor(..), (<$>))
import Data.Char (toUpper)

class Functor f where
  {-# MINIMAL fmap #-}
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b -> f a -> f b
  --x <$ xs = fmap (const x) xs
  (<$) = fmap . const
  infixl 4 <$

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

instance Functor [] where
  fmap f [] = []
  fmap f (x : xs) = f x : fmap f xs

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just v) = Just (f v)

data Tree a = Empty
            | Leaf a
            | Branch (Tree a) a (Tree a)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch l a r) = Branch (fmap f l) (f a) (fmap f r)

doubleMap :: (Int -> Bool) -> [[Int]] -> [[Bool]]
doubleMap f xs = fmap (fmap f) xs

doubleMap' :: Functor f => (a -> b) -> f (f a) -> f (f b)
doubleMap' f xs = fmap (fmap f) xs

doubleMap'' :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
doubleMap'' = fmap . fmap

tripleMap :: (Functor f1, Functor f2, Functor f3)
          => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
tripleMap = fmap . fmap . fmap


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

doubleFirst :: [Int] -> Maybe Int
doubleFirst = fmap (*2) . safeHead
--doubleFirst xs = case safeHead xs of
--  Nothing -> Nothing
--  Just x -> Just (x * 2)

upcaseName :: [(String, Int)] -> [(String, Int)]
--upcaseName = fmap (first (fmap toUpper))
upcaseName = (fmap . first . fmap) toUpper

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)


