import Data.Monoid (First(..))
import Data.Foldable

find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p = getFirst . foldMap (First . toMaybe p)

-- Scala: Option.when(p(x))(x)
toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe p x
  | p x = Just x
  | otherwise = Nothing

safeMaximum :: (Foldable t, Ord a) => t a -> Maybe a
safeMaximum = getMaxMaybe . foldMap (MaxMaybe . Just)

newtype MaxMaybe a = MaxMaybe { getMaxMaybe :: Maybe a }

instance Ord a => Semigroup (MaxMaybe a) where
  MaxMaybe (Just x) <> MaxMaybe (Just y) = MaxMaybe (Just (x `max` y))
  MaxMaybe Nothing <> y = y
  x <> MaxMaybe Nothing = x

instance Ord a => Monoid (MaxMaybe a) where
  mempty = MaxMaybe Nothing

--safeMaximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
--safeMaximumBy cmp = _ . foldMap _
--safeMaximumBy cmp = foldl' (_) Nothing

