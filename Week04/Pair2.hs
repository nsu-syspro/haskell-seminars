module Pair2 where

import Prelude hiding (Eq(..))

import TypeClasses

instance (Eq a, Eq b) => Eq (a, b) where
  (a, b) == (c, d) = a == c && b == d
