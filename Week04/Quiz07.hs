import Data.Maybe (maybeToList)

f1 :: (b -> c) -> (a -> b) -> a -> c
f1 = (.)


h1, h2 :: a -> Maybe a
h1 = Just
h2 = const Nothing

-- Data.Maybe.mapMaybe
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = concatMap (maybeToList . f)
