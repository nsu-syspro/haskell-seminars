import Prelude hiding (Functor(..), (<$>), fmap)

import Data.Char (toUpper)

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

-- Can this be made into Functor?
newtype B r a = B (a -> r)

-- Can't be turned into Functor:
--
--instance Functor (B r) where
--  fmap :: (a -> b) -> B r a -> B r b
--  fmap f (B g) = B (\b -> _)
  --   f :: a -> b
  --   g :: a -> r
  -- Want:
  --   _ :: b -> r
  -- Need:
  --   f' :: b -> a

-- Continuation monad
--
-- See https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Cont.html
--
newtype C r a = C ((a -> r) -> r)

instance Functor (C r) where
  fmap :: (a -> b) -> C r a -> C r b
  fmap f (C g) = C (\b2r -> g (b2r . f))
  --   f :: a -> b
  --   g :: (a -> r) -> r
  --   b2r :: b -> r
  -- Want:
  --   _ :: a -> r

-- Let's check Law (1) -- see below
--
--   fmap id (C g)
-- = C (\b2r -> g (b2r . id))
-- = id (C (\b2r -> g (b2r . id)))
-- = id (C (\b2r -> g b2r))
--   { b2r :: a -> r; g :: (a -> r) -> r }
-- = id (C g)

doubleMap :: (Int -> Bool) -> [[Int]] -> [[Bool]]
doubleMap f = fmap (fmap f)

doubleMap' :: Functor f => (a -> b) -> f (f a) -> f (f b)
doubleMap' f = fmap (fmap f)

doubleMap'' :: (Functor f1, Functor f2) =>
  (a -> b) -> f1 (f2 a) -> f1 (f2 b)
doubleMap'' = fmap . fmap

-- >>> doubleMap'' (+1) [("Alice", 7),("Bob", 3)]
-- [("Alice",8),("Bob",4)]

--tripleMap = fmap . fmap . fmap

upcaseNames :: [(String, Int)] -> [(String, Int)]
upcaseNames = (fmap . first . fmap) toUpper
--upcaseNames = fmap (first (fmap toUpper))

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

-- * Laws
--
--   (1) fmap id = id
--   (2) fmap f . fmap g = fmap (f . g)
--
-- Moreover, in Haskell (2) can be derived from (1)
-- Due to "parametricity"

-- * Parametricity and free theorems
--
-- Consider following function:
--
--   f :: [a] -> [a]
--
-- Representatives:
--
--   id :: [a] -> [a]
--   const [] :: [a] -> [a]
--   drop 2 :: [a] -> [a]
--   take 2 :: [a] -> [a]
--   reverse :: [a] -> [a]
--
-- Free theorem for `f :: [a] -> [a]`
--
--   forall g :: a -> b
--     map g . f = f . map g
--

-- * Free theorem for `map :: (a -> b) -> [a] -> [b]`
--
--   (free)
--   forall map' :: (a -> b) -> [a] -> [b]
--     if   a . b = c . d
--     then map a . map' b = map' c . map d


-- * Proof that (2) can be derived from (1)
--
-- Suppose we have (1) law for map:
--   
--   (1) map id = id
--
-- Then using (free) and 
--   a = f
--   b = g
--   c = id
--   d = (f . g)
--   map' = map
--
--
--   map f . map g
-- = map id . map (f .g) -- (free)
-- = id . map (f . g)    -- (1)
-- = map (f . g)
--
-- So 
--
--   (2) map f . map g = map (f . g)
--


-- * Uniqueness
--
-- If fmap satisfies (1) (and (2)) then it is unique
--
-- meaning that forall fmap' :: (a -> b) -> f a -> f b
-- which also satify (1) (and (2)):
--
--   fmap' ~ fmap
--
-- Proof:
--
-- Suppose there is fmap' != fmap that satisfies (1)
--
--   fmap' id
-- = id        -- (1) fmap'
-- = fmap id   -- (1) fmap
--


-- Examples of unlawful functors:
--
--
-- Breaks both laws:
--
--   instance Functor [] where
--     fmap f [] = []
--     fmap f (x : xs) = f x : f x : fmap f xs
-- 
-- Breaks (1), but not (2):
--
--   instance Functor Maybe where
--     fmap f Nothing = Nothing
--     fmap f (Just v) = Nothing --Just (f v)
--
-- Breaks (2), but not (1):
--
--   We need 'bottom' (undefined, error etc)
--   see https://github.com/quchen/articles/blob/master/second_functor_law.md#bottom-ruins-the-party


