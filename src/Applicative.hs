import Prelude hiding (Applicative(..))

import Text.Read (readMaybe)

-- How can we implement this function?
--
--fmap2 :: Functor f => (a -> (b -> c)) -> f a -> f b -> f c
--fmap2 g fa fb = _ (fmap g fa) fb
--
--  -- (fmap g fa) :: f (b -> c)
--  -- Want _ :: f (b -> c) -> f b -> f c
--
-- We can't! Functor is too weak


class Functor f => Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  infixl 4 <*>, <*, *>

  pure :: a -> f a

  -- fmap :: (a -> b) -> f a -> f b

  -- ap function (or splat)
  --
  -- Also used for sequencing computations
  (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = liftA2 ($)
  --(<*>) = liftA2 id
  --(<*>) = liftA2 (\g a -> g a)
  --(<*>) fg fa = liftA2 (\g a -> g a) fg fa
  -- liftA2 :: ((a -> b) -> a -> b) -> f (a -> b) -> f a -> f b

  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 g fa fb = g <$> fa <*> fb
  -- (fmap g fa) :: f (b -> c)

  -- Combines both contexts (or computations)
  -- but discards the second *result*
  (<*) :: f a -> f b -> f a
  (<*) = liftA2 const
  -- This is not correct:
  --(<*) = const

  -- Combines both contexts (or computations)
  -- but discards the first *result*
  (*>) :: f a -> f b -> f b
  (*>) = liftA2 (flip const)

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 g fa fb fc = g <$> fa <*> fb <*> fc
  -- g :: a -> b -> c -> d
  -- g <$> fa :: f (b -> c -> d)
  -- g <$> fa <*> fb :: f (c -> d)
  -- g <$> fa <*> fb <*> fc :: f d

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Just f <*> Just x  = Just (f x)
  _ <*> _ = Nothing
  --Nothing <*> Just x = Just _
  -- x :: a
  -- _ :: b
  --Just f <*> Nothing = Just _
  -- f :: a -> b
  -- _ :: b

-- * Applicative lists
--
-- There are two ways to implement applicative for list
--
-- [(+2),(*2)] <*> [1,2,3] = ?
--
--   (1)   (+2) <$> [1,2,3] ++ (*2) <$> [1,2,3]
--       ~ [3,4,5,2,4,6]
--   (2)   zipWith ($) [(+2),(*2)] [1,2,3]
--       ~ [3,4]
--        
-- This one only works for functions (a -> a) aka endomorphisms
--
--   (3)   ((+2) . (*2)) <$> [1,2,3]
--       ~ [4,6,8]

instance Applicative [] where
  pure x = [x]
  (<*>) :: [a -> b] -> [a] -> [b]
  fs <*> xs = [ f x | f <- fs, x <- xs ]

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving Show

instance Functor ZipList where
  --fmap f = ZipList . map f . getZipList
  fmap f x = pure f <*> x

instance Applicative ZipList where
  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)
  --(<*>) = ZipList . zipWith ($)
  -- We need some way to do something like this:
  --
  --   zipWith ($) :: [a -> b] -> [a] -> [b]
  --   getZipList :: ZipList a -> [a]
  --   _ :: ZipList (a -> b) -> ZipList a -> [b]

  pure :: a -> ZipList a
  pure x = ZipList (repeat x)
  --pure x = ZipList [x]
  -- This fails Identity for
  --
  --   pure id <*> [1,2,3] = [1]

-- This one only works for functions (a -> a) aka endomorphisms
--
--newtype RustamList a = RustamList { getRustam :: [a] }
--  deriving Show
--
--instance Functor RustamList where
--  fmap f x = pure f <*> x
--
--instance Applicative RustamList where
--  RustamList fs <*> RustamList xs = RustamList 

-- Reader functor/monad
instance Applicative ((->) e) where
  --(<*>) :: f (a -> b) -> f a -> f b
  (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
  f <*> g = \e -> f e (g e)
  -- f :: e -> (a -> b)
  -- g :: e -> a
  -- g e :: a
  -- f e :: a -> b
  -- (f e) (g e) :: b

  pure :: a -> (e -> a)
  pure = const

-- Writer functor/monad
instance Monoid e => Applicative ((,) e) where
  (<*>) :: (e, a -> b) -> (e, a) -> (e, b)
  (e1, f) <*> (e2, x) = (e1 <> e2, f x)

  pure :: a -> (e, a)
  pure x = (mempty, x)

instance Applicative (Either e) where
  pure = Right
  Right f <*> Right x = Right (f x)
  Left e <*> _ = Left e
  _ <*> Left e = Left e


-- * Laws
--
-- - Identity
--
--   pure id <*> v = v    or    id <$> v = v
--
-- - Composition
--
--   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--     or
--   (.) <$> u <*> v <*> w = u <*> (v <*> w)
--
-- - Homomorphism
--
--   pure f <*> pure x = pure (f x)
--     or
--   f <$> pure x = pure (f x)
--
-- - Interchange
--
--   u <*> pure y = pure ($ y) <*> u
--     or
--   u <*> pure y = ($ y) <$> u
--
--
-- Laws describe algorithm of moving all pure arguments
-- to the left of
--   
--   a1 <*> a2 <*> ... <*> an
--
-- to become
--
--   pure f <*> a1' <*> a2' <*> ... <*> an'
--
-- So that all ai' will not have pure.

-- * Examples

data Student = Student String Int
  deriving Show

maybeStudent :: Maybe String -> Maybe Int -> Maybe Student
maybeStudent name grade = Student <$> name <*> grade

-- >>> parseStudent ["Alice", "5", "foo"]
-- Just (Student "Alice" 5)
-- >>> parseStudent ["Bob", "bar", "foo"]
-- Nothing
-- >>> parseStudent ["Bob"]
-- Nothing
parseStudent :: [String] -> Maybe Student
parseStudent (name : grade : _) =
  maybeStudent (pure name) (readMaybe grade)
--parseStudent (name : grade : _) =
--      Student
--  <$> pure name
--  <*> readMaybe grade
--parseStudent (name : grade : _) = case readMaybe grade of
--  Nothing -> Nothing
--  Just g  -> Just (Student name g)
parseStudent _ = Nothing
