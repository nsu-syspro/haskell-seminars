module StateT where
import MonadTransformers

--newtype State s a = State { runState :: s -> (a, s) }

-- * StateT
--
-- State transformer

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT st s = fst <$> runStateT st s

execStateT :: Functor m => StateT s m a -> s -> m s
execStateT st s = snd <$> runStateT st s

get :: Applicative m => StateT s m s
get = StateT $ \s -> pure (s, s)

put :: Applicative m => s -> StateT s m ()
put s = StateT $ \_ -> pure ((), s)

gets :: Applicative m => (s -> a) -> StateT s m a
gets f = StateT $ \s -> pure (f s, s)

modify :: Applicative m => (s -> s) -> StateT s m ()
modify f = StateT $ \s -> pure ((), f s)

-- * Instances

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor m => Functor (StateT s m) where
  fmap f st = StateT $ \s -> first f <$> (runStateT st s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

  StateT sf <*> StateT sx = StateT $ \s -> do
    -- sf s :: m (a -> b, s)
    (f, s')  <- sf s
    (x, s'') <- sx s'
    pure (f x, s'')

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sx >>= f = StateT $ \s -> do
    (x, s') <- sx s
    runStateT (f x) s'
    
-------------------------------
-- transformers-style

class MonadTrans trans where
  lift :: Monad m => m a -> trans m a

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift action = StateT $ \s -> (,s) <$> action

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift action = MaybeT (Just <$> action)
    
-------------------------------
-- MonadIO

class MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO (StateT s IO) where
  liftIO = lift

instance MonadIO IO where
  liftIO = id

-- Generalized version of putStrLn for any MonadIO
gPutStrLn :: MonadIO m => String -> m ()
gPutStrLn = liftIO . putStrLn

-------------------------------
-- Monad stack

-- IO -> Maybe -> State
-- s -> IO (Maybe (a, s))
type ComplexStateMonad s a = StateT s (MaybeT IO) a

-- IO -> Maybe -> State -> Maybe
-- s -> IO (Maybe (Maybe a, s))
type MoreComplexStateMonad s a = MaybeT (StateT s (MaybeT IO)) a

-- IO -> Maybe -> State -> Maybe -> State
-- TODO: figure out the signature inside
type EvenMoreComplexStateMonad s u a = StateT u (MaybeT (StateT s (MaybeT IO))) a

complexIO :: EvenMoreComplexStateMonad s u String
complexIO = lift $ lift $ lift $ lift getLine

moreComplexState :: MoreComplexStateMonad s s
moreComplexState = lift get

moreComplexState' :: MoreComplexStateMonad s s
moreComplexState' = get'

complexState :: EvenMoreComplexStateMonad s u s
complexState = lift $ lift get

-- This does not work:
--complexState' :: EvenMoreComplexStateMonad s u s
--complexState' = get'

-------------------------------
-- mtl-style

class MonadState s m where
  get' :: m s
  put' :: s -> m ()
  
instance Applicative m => MonadState s (StateT s m) where
  get' = get
  put' = put
  
instance (Monad m, MonadState s m) => MonadState s (MaybeT m) where
  get' = lift get'
  put' = lift . put'
