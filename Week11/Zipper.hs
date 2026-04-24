import State
import Data.List (find)
import Data.Maybe (fromJust)

data TreeDirection = TLeft | TRight
  deriving (Show)

newtype TreeZipper a = TZ { unTZ :: (Tree a, [(TreeDirection, a, Tree a)]) }
  deriving (Show)

treeZipper :: Tree a -> TreeZipper a
treeZipper tree = TZ (tree, [])

goLeft :: TreeZipper a -> TreeZipper a
goLeft (TZ (Empty, crumbs)) = TZ (Empty, crumbs)
goLeft (TZ (Leaf x, crumbs)) = TZ (Leaf x, crumbs)
goLeft (TZ (Branch l x r, crumbs)) = TZ (l, (TLeft, x, r) : crumbs)

goRight :: TreeZipper a -> TreeZipper a
goRight (TZ (Empty, crumbs)) = TZ (Empty, crumbs)
goRight (TZ (Leaf x, crumbs)) = TZ (Leaf x, crumbs)
goRight (TZ (Branch l x r, crumbs)) = TZ (r, (TRight, x, l) : crumbs)

goUp :: TreeZipper a -> TreeZipper a
goUp (TZ (tree, [])) = TZ (tree, [])
goUp (TZ (tree, b : bs)) = case b of
  (TLeft, x, r) -> TZ (Branch tree x r, bs)
  (TRight, x, l) -> TZ (Branch l x tree, bs)

-- >>> goRight $ goLeft $ treeZipper exampleTree
-- TZ {unTZ = (Leaf 'c',[(TRight,'b',Leaf 'a'),(TLeft,'d',Leaf 'e')])}
-- >>> treeZipper exampleTree -: goLeft -: goRight
-- TZ {unTZ = (Leaf 'c',[(TRight,'b',Leaf 'a'),(TLeft,'d',Leaf 'e')])}
-- >>> toTree $ goRight $ goLeft $ treeZipper exampleTree
-- Branch (Branch (Leaf 'a') 'b' (Leaf 'c')) 'd' (Leaf 'e')
toTree :: TreeZipper a -> Tree a
toTree tz =
  fst $ fromJust $
  find (null . snd) $
  map unTZ $
  iterate goUp tz

infixl 6 -:
(-:) :: TreeZipper a -> (TreeZipper a -> TreeZipper a) -> TreeZipper a
tz -: f = f tz

update :: Tree a -> TreeZipper a -> TreeZipper a
update tree (TZ (_, bs)) = TZ (tree, bs)

foo :: State (TreeZipper a) Int
foo = do
  modify (goRight . goLeft)
  pure 2

--type TreeZipper a = (Tree a, [(TreeDirection, a, Tree a)])
--
-- Lists are like degenerate Tree that only go to the right
--
-- So we don't need to remember direction (it is always Right)
-- and the other subtree (it is always empty)

type ListZipper a = ([a], [a])
--                   |    - reversed list backwards
--                   - remaining tail 


