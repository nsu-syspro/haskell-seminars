-- Parametric polymorphism

-- id
f1 :: a -> a
f1 x = x


p1, p2, p3, p4 :: (a, a) -> (a, a)
p1 (x, y) = (x, y)
p2 (x, y) = (y, x)
p3 (x, y) = (x, x)
p4 (x, y) = (y, y)

g1, g2 :: a -> a -> Bool
g1 x y = True
g2 x y = False

h1, h2, h3, h4 :: Eq a => a -> a -> Bool
h1 x y = True
h2 x y = False
h3 x y = x == y
h4 x y = x /= y -- not (x == y)
