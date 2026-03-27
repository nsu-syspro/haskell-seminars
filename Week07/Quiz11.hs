import Data.Monoid (First(..), Endo(..))
import Data.Foldable
import Data.Ord

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

-- >>> safeMaximumBy compare [1,2,3]
-- Just 3
-- >>> safeMaximumBy compare [3,2,1]
-- Just 3
-- >>> safeMaximumBy compare []
-- Nothing
-- >>> safeMaximumBy (comparing length) ["abc", "abcd", "ab"]
-- Just "abcd"
-- >>> safeMaximumBy (comparing (Down . length)) ["abc", "abcd", "ab"]
-- Just "ab"

safeMaximumBy :: forall t a. Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
--safeMaximumBy cmp = _ . foldMap _
--safeMaximumBy cmp = foldl' (_) Nothing
safeMaximumBy cmp xs = appEndo (foldMap (Endo . maybeMax . Just) xs) Nothing
  where
    maybeMax :: Maybe a -> (Maybe a -> Maybe a)
    maybeMax (Just x) (Just y)
      | cmp x y == GT = Just x
      | otherwise     = Just y
    maybeMax Nothing y = y
    maybeMax x Nothing = x

