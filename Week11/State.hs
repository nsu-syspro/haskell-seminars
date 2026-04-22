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

-- >>> numerate exampleTree
-- Branch (Branch (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)

numerate :: Tree a -> Tree Int
numerate tree = evalState (traverse (const step) tree) 1
  where
    step :: State Int Int
    step = do
      x <- get
      modify (+1)
      pure x
      
