import Data.Kind (Type)

data B a b = B (a b)

data C (a :: ((k1 -> Type) -> k2)) (b :: k3 -> Type) = C (b (a b))

data E (a :: k) = E
data F (a :: k -> Type) = F
data G (a :: (k -> Type) -> k') = G
