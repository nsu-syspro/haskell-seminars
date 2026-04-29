module MonadTransformers where

import Text.Read (readMaybe)
import Control.Monad (liftM, ap)
import Control.Monad.Identity

-- * Example

data Student =
  Student
  Integer -- ^ Id
  String  -- ^ Name
  Int     -- ^ Group

-- Maybe (IO Student)
--   ^ pure value
--       either Just computation that we can run
--       or Nothing
--
-- IO (Maybe Student)
--   ^ computation that might fail
--       either returns Just student after running
--       or Nothing after running

parseStudent :: IO (Maybe Student)
--parseStudent = Student
--  <$> parseStudentId
--  <*> parseStudentName
--  <*> parseStudentGroup
parseStudent = do
  mid <- parseStudentId
  case mid of
    Nothing -> pure Nothing
    Just sid -> do
      mname <- parseStudentName
      case mname of
        Nothing -> pure Nothing
        Just sname -> do
          mgroup <- parseStudentGroup
          case mgroup of
            Nothing -> pure Nothing
            Just sgroup -> pure (Just $ Student sid sname sgroup)


parseStudentId :: IO (Maybe Integer)
parseStudentId = fmap readMaybe getLine

parseStudentName :: IO (Maybe String)
parseStudentName = do
  name <- getLine
  pure $
    if not (null name)
    then Just name
    else Nothing

parseStudentGroup :: IO (Maybe Int)
parseStudentGroup = fmap readMaybe getLine

-- * MaybeIO

newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Functor MaybeIO where
  fmap = liftM

instance Applicative MaybeIO where
  pure :: a -> MaybeIO a
  pure = MaybeIO . pure . pure
  (<*>) = ap

instance Monad MaybeIO where
  (>>=) :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b
  MaybeIO ma >>= f = MaybeIO $ do
    a <- ma
    case a of
      Nothing -> pure Nothing
      Just x  -> runMaybeIO (f x)

-- * MaybeIO example

parseStudent' :: MaybeIO Student
parseStudent' = Student
  <$> MaybeIO parseStudentId
  <*> MaybeIO parseStudentName
  <*> MaybeIO parseStudentGroup

parseStudentId' :: MaybeIO Integer
parseStudentId' = MaybeIO $ fmap readMaybe getLine

-- * MaybeT
--
-- Monad transformer for Maybe

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
  fmap = liftM

instance Monad m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (<*>) = ap

instance Monad m => Monad (MaybeT m) where
  MaybeT ma >>= f = MaybeT $ do
    a <- ma
    case a of
      Nothing -> pure Nothing
      Just x  -> runMaybeT (f x)

-- Original monad is equivalent to transformer with Identity
type Maybe' a = MaybeT Identity a

-- MaybeIO analog using transformers
type MaybeIO' a = MaybeT IO a


