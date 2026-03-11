import Data.Function (on)
import Data.Ord

g1, g2, g3, g4 :: Ord a => (b -> a) -> (b -> b -> Ordering)
g1 f x y = compare (f x) (f y)
g2 = (compare `on`)
g3 f = compare `on` f
g4 = comparing

-- ghci> sortBy (compare `on` snd) [(1, "foo"), (2, "bar")]
-- [(2,"bar"),(1,"foo")]
-- ghci> sortBy (comparing snd) [(1, "foo"), (2, "bar")]
-- [(2,"bar"),(1,"foo")]
