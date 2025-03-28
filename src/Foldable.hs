{-# LANGUAGE NoStarIsType #-}

import Data.Semigroup
import Data.Monoid
import Prelude hiding (foldr, foldl, foldMap, Foldable, Foldable(..), concat, concatMap)
import Data.Kind (Type)

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- mconcat :: Monoid a => [a] -> a
--
-- All of these combine elements
--
-- f :: Monoid b => (a -> b) -> [a] -> b

--foldMap :: Monoid b => (a -> b) -> [a] -> b
--foldMap f = mconcat . map f

-- This is endomorphism (Endo)
--               |_______
--foldr :: (a -> (b -> b)) -> b -> [a] -> b
--foldr f s xs = appEndo (foldMap (Endo . f) xs) s

-- TODO
--foldl :: (b -> (a -> b)) -> b -> [a] -> b
--foldl f s xs = appEndo (foldMap (Endo . f) xs) s


-- Kinds
--
-- ghci> :kind Int
-- Int :: Type
-- ghci> :kind Maybe Int
-- Maybe Int :: Type
-- ghci> :kind Maybe
-- Maybe :: Type -> Type
-- ghci> :kind []
-- [] :: Type -> Type
-- ghci> :kind Either
-- Either :: Type -> Type -> Type
-- ghci> :kind (,)
-- (,) :: Type -> Type -> Type
-- ghci> :kind (->)
-- (->) :: Type -> Type -> Type

-- Foldable Overview
--
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Foldable.html#g:7

-- Traversable will be later

class Foldable t where
  {-# MINIMAL foldMap | foldr #-}

  foldMap :: Monoid b => (a -> b) -> t a -> b
  foldMap f = foldr (\x m -> f x <> m) mempty

  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f s xs = appEndo (foldMap (Endo . f) xs) s

  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl f s xs =
    (appEndo . getDual) (foldMap (Dual . Endo . flip f) xs) s

  -- Generalized mconcat
  fold :: Monoid a => t a -> a
  fold = foldMap id

  toList :: t a -> [a]
  toList = foldMap (\c -> [c])

  null :: t a -> Bool
  null = not . getAny . foldMap (Any . const True)

  -- Monoid homomorphism
  -- or in other words lists can simulate integers
  length :: t a -> Int
  length = getSum . foldMap (const (Sum 1))

  --elem :: Eq a => a -> t a -> Bool
  
  maximum :: Ord a => t a -> a
  --maximum = getBadMax . foldMap BadMax
  maximum t = case toList t of
    [] -> error "maximum []"
    [x] -> x
    (x: xs) -> x `max` maximum xs

  --minimum :: Ord a => t a -> a
  --sum :: Num a => t a -> a
  --product :: Num a => t a -> a

  -- How to implement these?
  --
  --foldr' :: (a -> b -> b) -> b -> t a -> b
  --foldl' :: (b -> a -> b) -> b -> t a -> b
  --foldMap' :: Monoid b => (a -> b) -> t a -> b

------------------
-- Extra functions

concat :: Foldable t => t [a] -> [a]
concat = fold

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap = foldMap

------------
-- Instances

instance Foldable ((,) a) where
  foldMap f (_, r) = f r

instance Foldable (Either a) where
  foldMap _ (Left a) = mempty
  foldMap f (Right x) = f x

instance Foldable Maybe where
  foldMap _ Nothing = mempty
  foldMap f (Just x) = f x


--------------
-- Binary Tree

instance Foldable [] where
  foldMap f = mconcat . map f

data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving (Show)
  
instance Foldable Tree where
  foldMap f Leaf = mempty
  foldMap f (Branch l a r) =
    foldMap f l <> f a <> foldMap f r
    --f a <> foldMap f l <> foldMap f r

exampleTree :: Tree Char
exampleTree = Branch
  (Branch (Branch Leaf 'h' Leaf) 'e' Leaf)
  'l'
  (Branch (Branch Leaf 'l' Leaf) 'o' Leaf)



