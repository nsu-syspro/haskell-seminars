import Prelude hiding (empty, (<|>))

class Applicative f => Alternative f where
  {-# MINIMAL empty, (<|>) #-}
  empty :: f a
  (<|>) :: f a -> f a -> f a

  -- Similar to a+ in regex
  --   a+ = aa*
  --
  -- Runs computation at least once until it fails
  some :: f a -> f [a]
  some x = (:) <$> x <*> many x

  -- Similar to a* in regex
  --   a* = aa...
  --
  -- Runs computation until it fails
  many :: f a -> f [a]
  many x = some x <|> pure []


