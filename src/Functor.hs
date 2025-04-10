import Prelude hiding (Functor(..), (<$>), fmap)

class Functor (f :: * -> *) where
  {-# MINIMAL fmap #-}

  --fmap :: (a -> b) -> (f a -> f b)
  fmap :: (a -> b) -> f a -> f b

  (<$) :: a -> f b -> f a
  (<$) = fmap . const

infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

instance Functor [] where
  --fmap = map
  fmap f [] = []
  fmap f (x : xs) = f x : fmap f xs

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just v) = Just (f v)

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead _       = Nothing

doubleFirst :: [Int] -> Maybe Int
doubleFirst = fmap (*2) . safeHead
--doubleFirst xs = (*2) <$> safeHead xs
--doubleFirst xs = safeHead xs <&> (*2)
--doubleFirst xs = case safeHead xs of
--  Nothing -> Nothing
--  Just x  -> Just (x * 2)

data Tree a = Leaf a | Branch (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch l a r) = Branch (fmap f l) (f a) (fmap f r)

-- Writer monad
instance Functor ((,) e) where
  fmap f (x, y) = (x, f y)

instance Functor (Either e) where
  fmap f (Left e)  = Left e
  fmap f (Right x) = Right (f x)

-- Reader monad
instance Functor ((->) e) where
  fmap :: (a -> b) -> (e -> a) -> (e -> b)
  fmap = (.)

-- specialization of (a,b) to (a,a)
data Point2D a = Point2D a a
 deriving Show

instance Functor Point2D where
  fmap :: (a -> b) -> Point2D a -> Point2D b
  fmap f (Point2D x y) = Point2D (f x) (f y)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

-- Can this be made into Functor?
newtype B r a = B ((a -> r) -> B r a)



-- * Laws
--
--   (1) fmap id = id
--   (2) fmap f . fmap g = fmap (f . g)
--
-- Moreover, in Haskell (2) can be derived from (1)
-- Due to "parametricity"
