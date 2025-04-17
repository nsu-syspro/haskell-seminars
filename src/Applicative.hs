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
