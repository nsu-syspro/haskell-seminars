module Bool where

import Prelude hiding (Bool (..), (&&), (||))

data Bool = False | True
  deriving (Show, Enum, Eq, Ord)

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False
--(&&) True True = True
--(&&) _    _    = False

infixr 2 ||

(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True
