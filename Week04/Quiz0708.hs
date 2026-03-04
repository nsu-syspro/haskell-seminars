import Data.Maybe (maybeToList)
import Data.Function (on)

f1 :: (b -> c) -> (a -> b) -> a -> c
f1 = (.)


h1, h2 :: a -> Maybe a
h1 = Just
h2 = const Nothing

-- Data.Maybe.mapMaybe
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = concatMap (maybeToList . f)

g1, g2, g3 :: Ord a => (b -> a) -> (b -> b -> Ordering)
g1 f x y = compare (f x) (f y)
g2 = (compare `on`)
g3 f = compare `on` f

-- ghci> sortBy (compare `on` snd) [(1, "foo"), (2, "bar")]
-- [(2,"bar"),(1,"foo")]
