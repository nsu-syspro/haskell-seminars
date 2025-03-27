{-# LANGUAGE NoStarIsType #-}

import Data.Monoid
import Prelude hiding (foldr, foldl, foldMap)
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

class Foldable (t :: Type -> Type) where
  foldMap :: Monoid a => t a -> a
  foldr :: (a -> b -> b) -> b -> t a -> b






