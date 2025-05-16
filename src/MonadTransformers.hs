module MonadTransformers where

import Control.Monad (liftM, ap)

-- Maybe (IO a)
-- IO (Maybe a)

-- Transformer (see transformers and mtl packages)
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Functor MaybeIO where
  fmap = liftM

instance Applicative MaybeIO where
  pure :: a -> MaybeIO a
  pure x = MaybeIO $ pure (pure x)
  (<*>) = ap

instance Monad MaybeIO where
  (>>=) :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b
  ma >>= f = MaybeIO $ do -- IO (Maybe a)
    a <- runMaybeIO ma
    case a of
      Just x -> runMaybeIO (f x)
      Nothing -> pure Nothing






