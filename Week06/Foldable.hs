import Prelude hiding (Foldable(..))

import Data.Monoid (Endo(..), Sum(..), Product(..), Dual(..))
import Data.Semigroup (Max(..))

class Foldable t where
  {-# MINIMAL foldMap | foldr #-}
  fold :: Monoid m => t m -> m
  fold = foldMap id

  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f = foldr (\a b -> f a <> b) mempty

  foldMap' :: Monoid m => (a -> m) -> t a -> m
  --foldMap' f = foldl' (\m a -> m <> f a) mempty
  --foldMap' f = foldr (\a m -> let !r = f a <> m in r) mempty
  --foldMap' f = foldr (\a m -> let r = f a in r `seq` r <> m) mempty
  foldMap' f = foldr (\a m -> (<> m) $! f a) mempty

  -- (a -> (b -> b))
  -- (a -> Endo (b -> b))
  foldr :: (a -> b -> b) -> b -> t a -> b
  --foldr f s xs = appEndo (foldMap (\a -> Endo (f a)) xs) s
  foldr f s xs = appEndo (foldMap (Endo . f) xs) s

--  foldr'   :: (a -> b -> b) -> b -> t a -> b

  foldl :: (b -> a -> b) -> b -> t a -> b
  --foldl f s xs = appEndo (getDual (foldMap (Dual . Endo . flip f) xs)) s
  foldl f = flip (appEndo . getDual . foldMap (Dual . Endo . flip f))

  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldl' f s xs = appEndo' (getDual (foldMap (Dual . Endo' . flip f) xs)) s

--  foldr1   :: (a -> a -> a) -> t a -> a
--  foldl1   :: (a -> a -> a) -> t a -> a

  toList :: t a -> [a]
  toList = foldMap (: [])

  null :: t a -> Bool
  null = not . getAny . foldMap (const (Any True))

  length  :: t a -> Int
  length = getSum . foldMap (const (Sum 1))
  
  elem    :: Eq a => a -> t a -> Bool
  elem v = getAny . foldMap (Any . (== v))

  maximum :: Ord a => t a -> a
  --maximum = getMax . foldMap Max
  maximum xs = case toList xs of
    [] -> error "..."
    (x:xs) -> foldl' max x xs

--  minimum :: Ord a => t a -> a

  sum :: Num a => t a -> a
  sum = getSum . foldMap Sum

  product :: Num a => t a -> a
  product = getProduct . foldMap Product

-- f = _ . foldMap _

-- * Monoids

newtype Any = Any { getAny :: Bool }
  deriving (Show, Eq)

instance Semigroup Any where
  Any x <> Any y = Any (x || y)

instance Monoid Any where
  mempty = Any False

newtype Endo' a = Endo' { appEndo' :: a -> a }

instance Semigroup (Endo' a) where
  Endo' f <> Endo' g =
    let f' !x = f x
        g' x = g x
    in  Endo' (f' . g')

instance Monoid (Endo' a) where
  mempty = Endo' id

-- * Instances

myconcat' :: Monoid m => [m] -> m
myconcat' = go mempty
  where
    go !acc [] = acc
    go !acc (x:xs) = go (x <> acc) xs

myconcat :: Monoid m => [m] -> m
myconcat [] = mempty
myconcat (x:xs) = x <> myconcat xs

instance Foldable [] where
  foldMap f = mconcat . map f

instance Foldable Maybe where
  foldMap f Nothing  = mempty
  foldMap f (Just v) = f v

-- String = [Char]
--
-- Doesn't work:
--instance Foldable String where
