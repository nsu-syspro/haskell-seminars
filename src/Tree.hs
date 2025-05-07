module Tree where

import Data.Traversable

-- * Tree

data Tree a = Empty | Leaf a | Branch (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse g Empty = pure Empty
  traverse g (Leaf a) = Leaf <$> g a
  traverse g (Branch l a r) = Branch <$> traverse g l <*> g a <*> traverse g r

exampleTree :: Tree String
exampleTree = Branch (Leaf "abc") "foo" (Branch (Leaf "c") "bar" Empty)
