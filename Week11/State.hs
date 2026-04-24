{-# LANGUAGE LambdaCase #-}
module State where

newtype Reader e a = Reader (e -> a)
newtype Writer e a = Writer (e, a)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

newtype State s a = State { runState :: s -> (a, s) }

evalState :: State s a -> s -> a
evalState st = fst . runState st
--evalState st s = fst (runState st s)

execState :: State s a -> s -> s
execState st = snd . runState st

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)
--put s = State $ \_ -> ((), s)

gets :: (s -> a) -> State s a
gets f = State $ \s -> (f s, s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

instance Functor (State s) where
  fmap f st = State $ \s -> first f (runState st s)
  --fmap f st = State (first f . runState st)
  --fmap f (State st) = State $ \s -> first f (st s)

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  --pure x = State (x,)
  --pure = State . (,)

  State sf <*> State sx = State $ \s ->
    -- sf :: s -> (a -> b, s)
    let (f, s')  = sf s
        (x, s'') = sx s'
    in  (f x, s'')

instance Monad (State s) where
  State sx >>= f = State $ \s ->
    let (x, s') = sx s
        -- sf :: State s b
        State sy = f x
    in  sy s'

-- * Example

data Tree a = Empty | Leaf a | Branch (Tree a) a (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

exampleTree :: Tree Char
exampleTree = Branch (Branch (Leaf 'a') 'b' (Leaf 'c')) 'd' (Leaf 'e')

-- Enumerates given tree in the order of traversal:
--
-- >>> numerate exampleTree
-- Branch (Branch (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)

numerate :: Tree a -> Tree Int
numerate tree = evalState traverseTree 1
  where
    step :: State Int Int
    step = get <* modify (+1)

    traverseTree :: State Int (Tree Int)
    traverseTree = traverse (const step) tree

-- >>> merge [1..] exampleTree
-- Branch (Branch (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)
-- >>> merge ['a'..'z'] exampleTree
-- Branch (Branch (Leaf 'a') 'b' (Leaf 'c')) 'd' (Leaf 'e')
-- >>> merge ['a','b','c'] exampleTree
-- *** Exception: Prelude.head: empty list

merge :: [b] -> Tree a -> Tree b
merge xs tree = evalState traverseTree xs
  where
    step :: State [b] b
    step = gets head <* modify tail

    traverseTree :: State [b] (Tree b)
    traverseTree = traverse (const step) tree

-- >>> mergeDefault 0 [1..] exampleTree
-- Branch (Branch (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)
-- >>> mergeDefault 'x' ['a','b','c'] exampleTree
-- Branch (Branch (Leaf 'a') 'b' (Leaf 'c')) 'x' (Leaf 'x')

mergeDefault :: b -> [b] -> Tree a -> Tree b
mergeDefault def input = merge (input ++ repeat def)

mergeDefault' :: forall a b. b -> [b] -> Tree a -> Tree b
mergeDefault' def input tree = evalState traverseTree input
  where
    step :: State [b] b
    step = do
      xs <- get
      case xs of
        (x : rest) -> x <$ put rest
        [] -> pure def

    traverseTree :: State [b] (Tree b)
    traverseTree = traverse (const step) tree

-- Idea:
--            Tree a -> Tree (Maybe b)
-- sequenceA: Tree (Maybe b) -> Maybe (Tree b)
--
-- >>> mergeMaybe [1..] exampleTree
-- Just (Branch (Branch (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5))
-- >>> mergeMaybe ['a','b','c'] exampleTree
-- Nothing
mergeMaybe :: forall a b. [b] -> Tree a -> Maybe (Tree b)
mergeMaybe input tree = sequenceA maybeTree
  where
    maybeTree :: Tree (Maybe b)
    maybeTree = evalState traverseTree input

    traverseTree :: State [b] (Tree (Maybe b))
    traverseTree = traverse (const step) tree

--    step :: State [b] (Maybe b)
--    step = do
--      xs <- get
--      case xs of
--        [] -> pure Nothing
--        (x:rest) -> Just x <$ put rest

    step :: State [b] (Maybe b)
    step = get >>= \case
      [] -> pure Nothing
      (x:rest) -> Just x <$ put rest


