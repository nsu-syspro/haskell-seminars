module Types where

-- Synonyms 
--
-- Erased on early compilation stages.
-- Denote *the same* type.
-- No type safety!
--
-- Used for saving characters on very long type signatures
-- and for documentation.

type IntPair = (Int, Int)

type StudentId = Int
type TeacherId = Int

t1 :: TeacherId
t1 = 24

inc :: StudentId -> StudentId
inc x = x + 1

-- Both allowed:

f1 :: TeacherId
f1 = inc t1

f2 :: StudentId
f2 = inc t1

-- Algebraic Data Type (ADT)

data Color = Red | Green | Blue
  deriving (Show, Eq)

data Color' where
  Red'   :: Color'
  Green' :: Color'
  Blue'  :: Color'
  deriving (Show)

isRed :: Color -> Bool
isRed Red = True
isRed _   = False

isGreen :: Color -> Bool
isGreen x = x == Green


data SafeDivResult = DivByZero | Result Integer



