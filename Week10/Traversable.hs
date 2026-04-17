import Prelude hiding (Traversable(..))
import Control.Monad.Identity (Identity(..))
import Data.Coerce (coerce)
import Control.Applicative (liftA, ZipList (..))
import Data.Functor (($>))

class (Functor t, Foldable t) => Traversable t where
  {-# MINIMAL (traverse | sequenceA) #-}

  traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id

  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  mapM = traverse

  sequence :: Monad m => t (m a) -> m (t a)
  sequence = sequenceA

-- * Defaults

fmapDefault :: Traversable f => (a -> b) -> f a -> f b
fmapDefault f = runIdentity . traverse (Identity . f)

foldMapDefault :: (Traversable f, Monoid m) => (a -> m) -> f a -> m
foldMapDefault f = getConst . traverse (Const . f)
-- traverse (Const . f) :: f a -> Const m (f b)
-- getConst :: Const m (f b) -> m

-- * Const functor
--
-- Note: Const cannot be Monad!

-- 'b' here is called phantom type parameter
newtype Const a b = Const { getConst :: a }
  deriving (Show)

-- This would require Monoid constraint!
--instance Monoid a => Functor (Const a) where
--  fmap = liftA

-- But this does not!
instance Functor (Const a) where
  fmap f = coerce

instance Monoid a => Applicative (Const a) where
  pure :: b -> Const a b
  pure _ = Const mempty

  (<*>) :: forall b c. Const a (b -> c) -> Const a b -> Const a c
  --Const x <*> Const y = Const (x <> y)
  --Const x <*> Const y = Const @a @c (x <> y)
  (<*>) = coerce ((<>) @a)

-- * Phantom type parameter example

data Dollar
data Ruble
data Yuan

newtype Currency v = Currency { getCurrency :: Int }
  deriving (Show, Num)

-- >>> addCurrency oneRuble oneRuble
-- Currency {getCurrency = 2}
-- >>> addCurrency oneRuble oneDollar
-- Couldn't match type `Dollar' with `Ruble'
-- Expected: Currency Ruble
--   Actual: Currency Dollar
-- In the second argument of `addCurrency', namely `oneDollar'
-- In the expression: addCurrency oneRuble oneDollar
-- In an equation for `it_aefuP':
--     it_aefuP = addCurrency oneRuble oneDollar
addCurrency :: Currency v -> Currency v -> Currency v
addCurrency = coerce ((+) @(Currency _))
--addCurrency (Currency x) (Currency y) = Currency (x + y)

oneRuble :: Currency Ruble
oneRuble = Currency 1

oneDollar :: Currency Dollar
oneDollar = Currency 1

-- * Instances

instance Traversable Maybe where
  sequenceA :: Applicative f => Maybe (f a) -> f (Maybe a)
  sequenceA Nothing = pure Nothing
  sequenceA (Just fa) = Just <$> fa

instance Traversable [] where
  sequenceA :: Applicative f => [f a] -> f [a]
  sequenceA [] = pure []
  sequenceA (fa:fas) = (:) <$> fa <*> sequenceA fas

-- * Tree

data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)
  deriving (Show)

exampleTree = Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c')

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Empty        = Empty
  fmap f (Leaf x)     = Leaf $ f x
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Branch l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f Empty        = pure Empty
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r

-- * Transpose matrix [[a]]

exampleMatrix = [[1,2,3],[4,5,6]]

squareMatrix = [[1,2,3],[4,5,6],[7,8,9]]

-- sequenceA does not work:
--
-- >>> sequenceA exampleMatrix
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

-- >>> sequenceA (fmap ZipList exampleMatrix)
-- ZipList {getZipList = [[1,4],[2,5],[3,6]]}
-- >>> traverse ZipList exampleMatrix
-- ZipList {getZipList = [[1,4],[2,5],[3,6]]}

-- >>> transpose squareMatrix
-- [[1,4,7],[2,5,8],[3,6,9]]

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

-- * Applicative trees

-- >>> exampleListTree
-- Branch (Branch (Leaf [1,2,3]) (Leaf [1,2,3])) (Leaf [1,2,3])
exampleListTree = [1,2,3] <$ exampleTree

-- >>> f1 exampleListTree
-- [Branch (Branch (Leaf 1) (Leaf 1)) (Leaf 1),Branch (Branch (Leaf 1) (Leaf 1)) (Leaf 2),Branch (Branch (Leaf 1) (Leaf 1)) (Leaf 3),Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 1),Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 2),Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3),Branch (Branch (Leaf 1) (Leaf 3)) (Leaf 1),Branch (Branch (Leaf 1) (Leaf 3)) (Leaf 2),Branch (Branch (Leaf 1) (Leaf 3)) (Leaf 3),Branch (Branch (Leaf 2) (Leaf 1)) (Leaf 1),Branch (Branch (Leaf 2) (Leaf 1)) (Leaf 2),Branch (Branch (Leaf 2) (Leaf 1)) (Leaf 3),Branch (Branch (Leaf 2) (Leaf 2)) (Leaf 1),Branch (Branch (Leaf 2) (Leaf 2)) (Leaf 2),Branch (Branch (Leaf 2) (Leaf 2)) (Leaf 3),Branch (Branch (Leaf 2) (Leaf 3)) (Leaf 1),Branch (Branch (Leaf 2) (Leaf 3)) (Leaf 2),Branch (Branch (Leaf 2) (Leaf 3)) (Leaf 3),Branch (Branch (Leaf 3) (Leaf 1)) (Leaf 1),Branch (Branch (Leaf 3) (Leaf 1)) (Leaf 2),Branch (Branch (Leaf 3) (Leaf 1)) (Leaf 3),Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1),Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 2),Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 3),Branch (Branch (Leaf 3) (Leaf 3)) (Leaf 1),Branch (Branch (Leaf 3) (Leaf 3)) (Leaf 2),Branch (Branch (Leaf 3) (Leaf 3)) (Leaf 3)]
-- >>> f2 exampleListTree
-- [Branch (Branch (Leaf 1) (Leaf 1)) (Leaf 1),Branch (Branch (Leaf 2) (Leaf 2)) (Leaf 2),Branch (Branch (Leaf 3) (Leaf 3)) (Leaf 3)]

f1 :: Tree [a] -> [Tree a]
f1 = sequenceA

f2 :: Tree [a] -> [Tree a]
f2 = getZipList . traverse ZipList

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  Empty <*> _ = Empty
  _ <*> Empty = Empty
  Leaf f <*> t = fmap f t
  Branch l r <*> t = Branch (l <*> t) (r <*> t)

-- >>> (id <$ exampleTree) <*> Empty
-- Empty
--
-- >>> g1 [Leaf 1, Branch (Leaf 2) Empty]
-- Branch (Leaf [1,2]) Empty
-- >>> g1 [Leaf 1, Branch (Leaf 2) Empty, Branch (Leaf 3) (Leaf 4)]
-- Branch (Branch (Leaf [1,2,3]) (Leaf [1,2,4])) Empty
-- >>> g1 [Leaf 1, Branch (Leaf 2) (Leaf 3)]
-- Branch (Leaf [1,2]) (Leaf [1,3])
-- >>> g1 [Leaf 1, Branch (Leaf 2) (Leaf 3), Branch (Leaf 4) (Leaf 5)]
-- Branch (Branch (Leaf [1,2,4]) (Leaf [1,2,5])) (Branch (Leaf [1,3,4]) (Leaf [1,3,5]))

g1 :: [Tree a] -> Tree [a]
g1 = sequenceA
