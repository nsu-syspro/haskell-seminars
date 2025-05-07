{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Prelude hiding (Traversable(..))
import Control.Applicative (Applicative(liftA2))
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

-- * Laws
--
-- - Naturality
--
--     t . traverse f = traverse (t . f)
--
-- - Identity
--
--     traverse Identity = Identity
--
-- - Composition
--
--     traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
--
-- Signatures:
--
--   (Traverse t, Applicative u, Applicative w) => 
--
--     traverse :: (a -> u b) -> t a -> u (t b)
--     f :: a -> u b
--
--     g :: b -> w c
--
--     fmap g . f :: a -> u (w c)
--
--     Applicative transformation from functor u to w
--     t :: u b -> w c
--
--     Compose :: u (w a) -> Compose u w a
--
--     We can guess that Compose data type is like this:
--
--       newtype Compose u w a = Compose (u (w a))
--
--     Like composition on functions:
--
--       (f . g) x = f (g x)
--     
--     traverse f :: t a -> u (t b)
--     fmap (traverse g) :: u (t b) -> u (w (t c))
--     fmap (traverse g) . traverse f :: t a -> u (w (t c))


newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  --fmap = coerce (fmap @f @(g a) @(g b) . fmap @g)
  --fmap f (Compose fga) = Compose (fmap (fmap f) fga)
  fmap f = Compose . fmap (fmap f) . getCompose

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  liftA2 :: (a -> b -> c) -> Compose f g a -> Compose f g b -> Compose f g c
  liftA2 f (Compose fga) (Compose fgb) = Compose ((liftA2 . liftA2) f fga fgb)

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose fga2b <*> Compose fga = Compose ((<*>) <$> fga2b <*> fga)
  -- (<*>) :: g (a -> b) -> (g a -> g b)
  -- fmap :: (a -> b) -> (f a -> f b)
  -- fmap (<*>) :: f (g (a -> b)) -> f (g a -> g b)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  --foldMap f (Compose fga) = foldMap (foldMap f) fga
  foldMap f = foldMap (foldMap f) . getCompose

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative u => (a -> u b) -> Compose f g a -> u (Compose f g b)
  --traverse f (Compose fga) = Compose <$> traverse (traverse f) fga
  traverse f = fmap Compose . traverse (traverse f) . getCompose
