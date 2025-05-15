import Tree

import Data.Traversable
import Data.Coerce (coerce)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

newtype Reader e a = Reader { runReader :: e -> a }
newtype Writer e a = Writer { runWriter :: (a, e) }
-- Applicative (Writer e) would require Monoid e

newtype State s a = State { runState :: s -> (a, s) }

evalState :: State s a -> s -> a
evalState sa s = fst $ runState sa s

execState :: State s a -> s -> s
execState sa s = snd $ runState sa s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- foo :: Map k v -> b -> (c, Map k v)
-- bar :: Map k v -> a -> (b, Map k v)
--
-- How to compose
--
-- foo . bar
--
-- With instead:
--
-- foo :: State (Map k v) (b -> c)
-- bar :: State (Map k v) (a -> b)
--
-- Can compose with Applicative
--
-- bar <*> foo <*> (.. :: State (Map k v) a)

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State sa) = State $ \s -> first f (sa s)
  --fmap f (State sa) = State $ \s ->
  --  let (a, s') = sa s
  --  in  (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  State sf <*> State sa = State $ \s ->
    let (f, s') = sf s
    in  first f (sa s') 
  --State sf <*> State sa = State $ \s ->
  --  let (f, s')  = sf s
  --      (a, s'') = sa s'
  --  in  (f a, s'') 

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State sa >>= f = State $ \s ->
    let (a, s') = sa s
        State sb = f a
    in  sb s'

-- Task
--
-- How can we turn Tree a -> Tree Int
-- so that it numerates each value 1, 2....
--
-- >>> numerate exampleTree
-- Branch (Leaf 1) 2 (Branch (Leaf 3) 4 Empty)
--

-- traverse :: (Traversable t, Applicative f) =>
--    (a -> f b) -> t a -> f (t b)

numerate :: Tree a -> Tree Int
numerate tree = evalState traverseTree 1
 where
  -- s -> (a, s)
  -- Int -> (Int, Int)
  step :: State Int Int
  step = get <* modify (+1)
  --step = State $ \s -> (s, s + 1)

  traverseTree :: State Int (Tree Int)
  traverseTree = traverse (\_ -> step) tree
  
