{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Prelude hiding (Traversable(..))
import Data.Coerce (coerce)
import Text.Read (readMaybe)

-- Let's try to implement following function:
--
-- >>> collectMaybe []
-- Just []
-- >>> collectMaybe [Just 1, Just 2, Just 3]
-- Just [1,2,3]
-- >>> collectMaybe [Nothing, Just 1, Just 2]
-- Nothing
collectMaybe :: [Maybe a] -> Maybe [a]
collectMaybe []        = Just []
collectMaybe (v : xs)  = (:) <$> v <*> collectMaybe xs

-- This does not work yet (because of (:) constructor)
--collectMaybe' :: Foldable t => t (Maybe a) -> Maybe (t a)
--collectMaybe' = foldr (\v -> (<*>) ((:) <$> v)) (Just [])

collectMaybe'' :: Applicative f => [f a] -> f [a]
collectMaybe'' = foldr (\v -> (<*>) ((:) <$> v)) (pure [])

-- Need something else -- Traversable

collectMaybe''' :: (Traversable t, Applicative f) =>
  t (f a) -> f (t a)
collectMaybe''' = sequenceA

class (Functor t, Foldable t) => Traversable t where
  {-# MINIMAL (traverse | sequenceA) #-}

  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id

  sequence :: Monad m => t (m a) -> m (t a)
  sequence = sequenceA

  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  mapM = traverse

instance Traversable Maybe where
  traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing  = pure Nothing
  traverse g (Just v) = Just <$> g v

instance Traversable [] where
  traverse _ []     = pure []
  traverse g (x:xs) = (:) <$> g x <*> traverse g xs


-- * Helper functors

newtype Identity a = Identity { runIdentity :: a }
  deriving (Show, Semigroup, Monoid)

instance Functor Identity where
  fmap :: (a -> b) -> (Identity a -> Identity b)
  fmap = coerce

instance Applicative Identity where
  pure = coerce
  (<*>) = coerce

instance Foldable Identity where
  foldMap = coerce

-- In following definition 'b' is called *phantom* type parameter
newtype Const a b = Const { getConst :: a }
  deriving (Show, Semigroup, Monoid)

instance Functor (Const e) where
  fmap :: (a -> b) -> Const e a -> Const e b
  fmap _ = coerce

instance Monoid e => Applicative (Const e) where
  pure :: a -> Const e a
  pure _ = mempty
  (<*>) :: Const e (a -> b) -> Const e a -> Const e b
  -- Requires ScopedTypeVariables:
  --(<*>) = coerce ((<>) :: e -> e -> e)
  -- Requires TypeApplications:
  (<*>) = coerce ((<>) @e)

-- * fmapDefault and foldMapDefault

fmapDefault :: forall t a b. Traversable t => (a -> b) -> t a -> t b
--fmapDefault g = runIdentity . traverse (Identity . g)
fmapDefault = coerce (traverse @t @Identity @a @b)

foldMapDefault :: forall t a m. (Traversable t, Monoid m) => (a -> m) -> t a -> m
--foldMapDefault g = getConst . traverse (Const . g)
foldMapDefault = coerce (traverse @t @(Const m) @a @m)

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

-- * Coerce

newtype FirstName = FirstName String deriving Show
newtype LastName = LastName String deriving Show
newtype FullName = FullName String deriving Show

fullName :: FirstName -> LastName -> FullName
fullName = coerce (\f l -> f ++ " " ++ l)
  -- coerce is specialized to
  --   (String -> String -> String) -> 
  --    (FirstName -> LastName -> FullName)

weird :: FirstName -> LastName
weird = coerce

-- This results in compiler error
--weird2 :: FirstName -> Bool
--weird2 = coerce

-- >>> plusTen "10.0"
-- Just "20.0"

plusTen :: String -> Maybe String
plusTen s = case readMaybe @Double s of
  Just v -> Just $ show (v + 10)
  Nothing -> Nothing

