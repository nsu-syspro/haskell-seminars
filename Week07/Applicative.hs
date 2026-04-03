import Prelude hiding (Applicative(..))

class Functor f => Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  pure :: a -> f a

  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b
  --fg <*> fa = liftA2 ($) fg fa
  (<*>) = liftA2 ($)

  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  --liftA2 g fa fb = pure g <*> fa <*> fb
  liftA2 g fa fb = g <$> fa <*> fb

  (*>) :: f a -> f b -> f b
  (*>) = liftA2 (\_ b -> b)
  --fa *> fb = liftA2 (\_ b -> b) fa fb
  --fa *> fb = (\_ b -> b) <$> fa <*> fb

  (<*) :: f a -> f b -> f a
  --fa <* fb = liftA2 const fa fb
  (<*) = liftA2 const

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 g fa fb fc = g <$> fa <*> fb <*> fc

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f fa = pure f <*> fa

instance Applicative Maybe where
  pure = Just
  Just g <*> Just x = Just (g x)
  _ <*> _ = Nothing

instance Applicative [] where
  fs <*> xs = [ f x | f <- fs, x <- xs]
  pure x = [x]

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
  fmap = liftA

instance Applicative ZipList where
  ZipList fs <*> ZipList xs = ZipList $ zipWith ($) fs xs
  pure x = ZipList (repeat x)

-- Writer functor/monad ~~> (e, a)
instance Monoid e => Applicative ((,) e) where
  pure x = (mempty, x)
  (e1, f) <*> (e2, x) = (e1 <> e2, f x) 

-- Reader functor/monad ~~> (e -> a)
instance Applicative ((->) e) where
  pure :: a -> (e -> a)
  pure = const
  (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
  fg <*> fa = \e -> fg e (fa e)

-- State monad ~~> (s -> (a, s))
--
-- get    :: State s s
-- gets   :: (s -> a) -> State s a
-- put    :: s -> State s ()
-- modify :: (s -> s) -> State s ()
--
-- https://hackage-content.haskell.org/package/mtl/docs/Control-Monad-State.html

data Student = Student String Int

mkStudent :: Maybe String -> Maybe Int -> Maybe Student
mkStudent name group = Student <$> name <*> group

mkStudent' :: Maybe String -> Int -> Maybe Student
mkStudent' name group = Student <$> name <*> pure group

mkStudent'' :: String -> Maybe Int -> Maybe Student
mkStudent'' name group = Student name <$> group

--data Tree a = Empty | Leaf a | Branch (Tree a) a (Tree a)





