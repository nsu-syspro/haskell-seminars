data Pair a = Pair a a
  deriving (Show)

instance Functor Pair where
  fmap f (Pair x y)= Pair (f x) (f y)

instance Foldable Pair where
  foldMap f (Pair x y) = f x <> f y

instance Traversable Pair where
  traverse :: Applicative f => (a -> f b) -> Pair a -> f (Pair b)
  traverse f (Pair x y) = Pair <$> f x <*> f y

instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair x y = Pair (f x) (g y)

-- Pair is similar to 2-element ZipList

-- * Can we define Monad Pair?
-- 
-- Let's look at join:
--
-- join :: Pair (Pair a) -> Pair a
--
-- Example:
--
--   Pair (Pair 1 2) (Pair 3 4)
-- ~ Pair x y -- x, y :: Int
--
-- ...
--
-- join should work like this
--
-- join (Pair (Pair a b) (Pair c d)) = Pair a d
--

instance Monad Pair where
  Pair x y >>= f =
    let Pair a b = f x
        Pair c d = f y
    in  Pair a d

ap :: Monad m => m (a -> b) -> m a -> m b
ap mab ma = mab >>= (\ab -> ma >>= pure . ab)

-- ma :: Pair a
-- pure . ab :: a -> Pair b

-- But can we turn ZipList into Monad?
--
-- The problem is the length of inner lists
--
--   [[1,2,3],[],[2,3]]
--
-- We can't take all diagonal elements.


