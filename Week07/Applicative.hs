import Prelude hiding (Applicative(..))

class Functor f => Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  pure :: a -> f a

  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b
  --fg <*> fa = liftA2 ($) fg fa
  (<*>) = liftA2 ($)

  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  --liftA2 g fa fb = pure g <*> fa <*> fb
  liftA2 g fa fb = g <$> fa <*> fb

  (*>) :: f a -> f b -> f b
  _ *> fb = fb

  --(<*) :: f a -> f b -> f a

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 g fa fb fc = g <$> fa <*> fb <*> fc

instance Applicative Maybe where
  pure = Just
  Just g <*> Just x = Just (g x)
  _ <*> _ = Nothing

data Student = Student String Int

mkStudent :: Maybe String -> Maybe Int -> Maybe Student
mkStudent name group = Student <$> name <*> group

mkStudent' :: Maybe String -> Int -> Maybe Student
mkStudent' name group = Student <$> name <*> pure group

mkStudent'' :: String -> Maybe Int -> Maybe Student
mkStudent'' name group = Student name <$> group




