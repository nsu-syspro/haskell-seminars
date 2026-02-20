{-# LANGUAGE RecordWildCards #-}

langList :: [String]
langList =
  [ "Haskell"
  , "C++"
  , "Scala"
  , "Python"
  ]

-- Record syntax for ADT

data Point a = Point
  { pointX :: a
  , pointY :: a
  }
  deriving Show

-- Equivalent to:
--
--   data Point a = Point a a
--     deriving Show
--   
--   pointX :: Point a -> a
--   pointX (Point x _) = x
--   
--   pointY :: Point a -> a
--   pointY (Point _ y) = y

dist0 :: Point Double -> Double
dist0 (Point x y) = sqrt (x^2 + y^2)

dist0' :: Point Double -> Double
dist0' Point{..} = sqrt (pointX^2 + pointY^2)

shiftY :: Double -> Point Double -> Point Double
shiftY y p = p { pointY = pointY p + y }

shiftY' :: Double -> Point Double -> Point Double
shiftY' y p@(Point {..}) = p { pointY = pointY + y }

ySquared :: Point Double -> Double
ySquared (Point { pointY = y }) = y^2

-- Synonyms do not define new distinct types

-- type CartCoord = (Double, Double)
-- type PolarCoord = (Double, Double)

-- newtype defines new distinct type
-- which is type checked but also erased after type checking

newtype CartCoord = CartCoord (Double, Double)
  deriving Show

newtype PolarCoord = PolarCoord (Double, Double)
  deriving Show

ccoord :: CartCoord
ccoord = CartCoord (1, 2)

pcoord :: PolarCoord
pcoord = PolarCoord (1, 2)

cartAdd :: CartCoord -> CartCoord -> CartCoord
cartAdd (CartCoord (x1, y1)) (CartCoord (x2, y2)) = CartCoord (x1 + x2, y1 + y2)

polarToCoord :: PolarCoord -> CartCoord
polarToCoord (PolarCoord (r, t)) = CartCoord (r * cos t, r * sin t)

-- newtype is good for narrowing value range of some type
-- or giving additional semantics to existing type

newtype Natural = Natural Integer
  deriving Show

constrNatural :: Integer -> Natural
constrNatural n
  | n < 0     = error "Negative number!"
  | otherwise = Natural n

constrNaturalMay :: Integer -> Maybe Natural
constrNaturalMay n
  | n < 0     = Nothing
  | otherwise = Just (Natural n)

data NEList a = Single a | Cons a (NEList a)
  deriving Show

