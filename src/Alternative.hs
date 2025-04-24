import Control.Applicative (liftA2)

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

  -- Similar to + in regexp ~ a+ matches a, aa, ...
  --   a+ = aa*
  -- Runs computation at least once until it fails
  some :: f a -> f [a]
  some fa = (:) <$> fa <*> many fa
  --some fa = liftA2 (:) fa (many fa)

  -- Similar to * in regexp ~ a* matches e, a, aa, ...
  -- Runs computation until it fails
  many :: f a -> f [a]
  many fa = some fa <|> pure []

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> y = y
  x       <|> _ = x

instance Alternative [] where
  empty = []
  (<|>) = (<>)
  --[] <|> xs = xs
  --xs <|> _  = xs
  

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty
