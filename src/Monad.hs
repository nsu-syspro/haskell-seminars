import Prelude hiding (Monad, (>>=))

import Data.Functor.Identity

infixl 1 >>=
infixr 1 >=>

class Applicative m => Monad m where
  {-# MINIMAL ((>>=) | (>=>) | join) #-}

  ------------------------------------------
  -- Notice any similarities
  --
  -- fmap  ::   (a ->   b) -> f a -> f b
  -- (<*>) :: f (a ->   b) -> f a -> f b
  -- (=<<) ::   (a -> m b) -> m a -> m b
  
  ------------------------------------------
  -- | bind operator
  (>>=) :: m a -> (a -> m b) -> m b

  -- >>= via >=>
  --ma >>= f = (f >=> g) ma
  --  where
  --    g :: m a -> m a
  --    g = id
  -- ma :: m a
  -- f :: a -> m b
  --
  -- want :: m a -> m b
  -- (>=>) :: (?? -> m b) -> (m a -> m ??) -> (m a -> m b)
  -- (>=>) :: (a -> m b) -> (m a -> m a) -> (m a -> m b)
  -- 

  -- >>= via join
  ma >>= f = join (fmap f ma)
  -- fmap f ma :: m (m b)

  ------------------------------------------
  -- | Kleisli composition aka "fish"
  --
  -- Functions with signature like _ :: a -> m b
  -- are called Kleisli arrows
  --
  -- So 'pure' is an example such arrow
  (>=>) :: (b -> m c) -> (a -> m b) -> (a -> m c)
  
  -- >=> via >>=
  f >=> g = \a -> g a >>= f

  -- >=> via join
  --f >=> g = \a -> join (fmap f (g a))

  ------------------------------------------
  -- Combines computations from nested monads
  join :: m (m a) -> m a
  
  -- join via >>=
  --join mma = mma >>= id
  --join = (>>= id)
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- a is m a
  -- b is a
  -- (>>=) :: m (m a) -> (m a -> m a) -> m a

  -- join via >=>
  join = f >=> g
    where
      f :: m a -> m a
      f = id
      g :: m (m a) -> m (m a)
      g = id
  -- (>=>) :: (b -> m c) -> (a -> m b) -> (a -> m c)
  -- a is m (m a)
  -- c is a
  -- b is m a
  --
  -- (>=>) :: (?? -> m a) -> (m (m a) -> m ??) -> (m (m a) -> m a)
  -- (>=>) :: (m a -> m a) -> (m (m a) -> m (m a)) -> (m (m a) -> m a)

  ------------------------------------------
  -- Additional methods in Prelude Monad
  (>>) :: m a -> m b -> m b
  ma >> mb = ma >>= \_ -> mb

  -- For historical reasons there is the following
  -- method in Monad that you should not use
  return :: a -> m a
  return = pure

-- * Default implementations for Functor and Applicative
-- based on Monad

-- (>>=) :: m a -> (a -> m b) -> m b

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ma >>= pure . f

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = mf >>= \f -> liftM f ma
-- f :: a -> b
-- want :: m b


instance Monad Identity where
  join (Identity m) = m

instance Monad Maybe where
  join Nothing = Nothing
  join (Just x) = x

  Nothing >>= _ = Nothing
  Just x  >>= f = f x

instance Monad [] where
  join = concat


-- * Do-notation (see App.hs)


