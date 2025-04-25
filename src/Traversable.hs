{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

import Prelude hiding (Traversable(..))
import Data.Coerce (coerce)

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

instance Functor (Const a) where
  fmap = coerce

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

