import Prelude hiding (Monad(..), (=<<))

class Applicative m => Monad m where
  {-# MINIMAL (>>=) #-}
  (>>=) :: m a -> (a -> m b) -> m b

  infixl 1 >>=

  (>>) :: m a -> m b -> m b
  (>>) = (*>)

  infixl 1 >>

  return :: a -> m a
  return = pure

join :: Monad m => m (m a) -> m a
join mma = mma >>= id

infixr 1 >=>

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
--f >=> g = \a -> f a >>= (\b -> g b)
f >=> g = \a -> f a >>= g

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

-- fmap
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ma >>= (pure . f)
--liftM = (=<<) . (pure .)
--liftM f ma = ma >>= (\a -> pure (f a))

-- (<*>)
ap :: Monad m => m (a -> b) -> m a -> m b
ap mab ma = mab >>= (\ab -> ma >>= pure . ab)
--ap mab ma = mab >>= (\ab -> ma >>= (\a -> pure (ab a)))

newtype Identity a = Identity { getIdentity :: a }
  deriving (Show)

instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  (<*>) = ap
  pure = Identity

instance Monad Identity where
  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  --(>>=) = flip (. getIdentity)
  --x >>= f = f (getIdentity x)
  Identity x >>= f = f x

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just a  >>= f = f a

instance Monad [] where
  --[]       >>= _ = []
  --(x : xs) >>= f = f x ++ (xs >>= f)
  (>>=) = flip foldMap


