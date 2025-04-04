{-# LANGUAGE NoStarIsType #-}

import Data.Semigroup hiding (Any, getAny)
import qualified Data.Semigroup as S
import Data.Monoid hiding (Any, getAny)
import qualified Data.Monoid as M
import Prelude hiding (foldr, foldl, foldMap, foldl', foldr', foldMap', Foldable, Foldable(..), concat, concatMap, any)
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

  foldMap' :: Monoid b => (a -> b) -> t a -> b
  --foldMap' f = foldr (\x m -> let r = f x in r `seq` r <> m) mempty
  foldMap' f = foldl' (\m x -> m <> f x) mempty
  
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f s xs = appEndo (foldMap (Endo . f) xs) s

  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl f s xs =
    (appEndo . getDual) (foldMap (Dual . Endo . flip f) xs) s

  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldr' f s xs = appEndo' (foldMap' (Endo' . f) xs) s

  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldl' f s xs =
    (appEndo' . getDual) (foldMap (Dual . Endo' . flip f) xs) s

  -- Generalized mconcat
  fold :: Monoid a => t a -> a
  fold = foldMap id

  toList :: t a -> [a]
  toList = foldMap (\c -> [c])

  null :: t a -> Bool
  --null = not . getAny . foldMap (Any . const True)
  null = not . any (const True)

  -- Monoid homomorphism
  -- or in other words lists can simulate integers
  length :: t a -> Int
  length = getSum . foldMap (const (Sum 1))

  elem :: Eq a => a -> t a -> Bool
  --elem x = getAny . foldMap (Any . (== x))
  elem x = any (== x)
  
  maximum :: Ord a => t a -> a
  --maximum = getBadMax . foldMap BadMax
  maximum t = case toList t of
    [] -> error "maximum []"
    [x] -> x
    (x: xs) -> x `max` maximum xs

  --minimum :: Ord a => t a -> a
  --sum :: Num a => t a -> a

  product :: Num a => t a -> a
  product = getProduct . foldMap Product

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

any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny . foldMap (Any . p)

find :: (Foldable t) => (a -> Bool) -> t a -> Maybe a
find p = M.getFirst . foldMap (M.First . toMaybe p)
 where
  toMaybe p a = if p a then Just a else Nothing

safeMaximum :: (Foldable t, Ord a) => t a -> Maybe a
safeMaximum = (maybe Nothing (Just . getMax)) . foldMap (Just . Max)



------------
-- Monoids

newtype Any = Any { getAny :: Bool }
  deriving Show

instance Semigroup Any where
  -- This will not short-circuit
  --Any True  <> Any True  = Any True
  --Any True  <> Any False = Any True
  Any True  <> Any _ = Any True
  Any False <> r     = r

instance Monoid Any where
  mempty = Any False

newtype Endo' a = Endo' { appEndo' :: a -> a }

instance Semigroup (Endo' a) where
  Endo' f <> Endo' g =
    let f' !x = f x
        g' !x = g x
    in  Endo' (\y -> f' (g' y))
    --Endo' (\x ->
    --  let g' = g x
    --  in  g' `seq` f g'
    --  )
  -- (<>) = (.)

instance Monoid (Endo' a) where
  mempty = Endo' id

-- Example of using bang pattern for strict evaluation
--f x y = let !s = (x + y) in s

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

instance Foldable [] where
  foldMap f = mconcat . map f

--------------
-- Binary Tree

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


-- Catamorphism
--
-- Generalizes recursion over ADT
--
-- If you pass ADT constructors to catamorphism
-- you will get the same structure
--
-- In Tree example:
--
--   treeFold Leaf Branch = id

-- foldr :: (a -> b -> b) -> t a -> b
treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold lv bf Leaf = lv
treeFold lv bf (Branch l v r) =
  let lr = treeFold lv bf l
      rr = treeFold lv bf r
  in bf lr v rr

treeDepth :: Tree a -> Int
treeDepth = treeFold 0 (\l _ r -> 1 + max l r)

treeDepth' :: Tree a -> Int
treeDepth' Leaf = 0
treeDepth' (Branch l _ r) = 1 + (treeDepth' l `max` treeDepth' r)


