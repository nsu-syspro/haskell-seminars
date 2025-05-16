{-# LANGUAGE FunctionalDependencies #-}

module StateT where

import Control.Monad.Identity (Identity)
import Control.Monad (liftM, ap)

-- newtype State s a = State { runState :: s -> (a, s) }

type State s a = StateT s Identity a

-- State transformer
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT sa s = fst <$> runStateT sa s

execStateT :: Functor m => StateT s m a -> s -> m s
execStateT sa s = snd <$> runStateT sa s

--get :: Monad m => StateT s m s
--get = StateT $ \s -> pure (s, s)
--
--put :: Monad m => s -> StateT s m ()
--put s = StateT $ \_ -> pure ((), s)

--modify :: Monad m => (s -> s) -> StateT s m ()
--modify f = get >>= \x -> put (f x)
--
--gets :: Monad m => (s -> a) -> StateT s m a
--gets f = f <$> get

instance Monad m => Functor (StateT s m) where
  fmap = liftM

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) = ap

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  sa >>= f = StateT $ \s -> do
    (a, s') <- runStateT sa s
    runStateT (f a) s'
      
-- See https://www.williamyaoh.com/posts/2023-06-10-monad-transformers-101.html

------------------------------------------
-- transformers-style

class MonadTrans trans where
  lift :: Monad m => m a -> trans m a

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma = StateT $ \s -> ma >>= \a -> pure (a, s)

------------------------------------------
-- mtl-style

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

modify :: MonadState s m => (s -> s) -> m ()
modify f = get >>= \x -> put (f x)

gets :: MonadState s m => (s -> a) -> m a
gets f = f <$> get

instance Monad m => MonadState s (StateT s m) where
  get :: StateT s m s
  get = StateT $ \s -> pure (s, s)
  
  put :: s -> StateT s m ()
  put s = StateT $ \_ -> pure ((), s)


------------------------------------------
-- MonadIO

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO (StateT s IO) where
  liftIO = lift 




