-- TODO: https://abhiroop.github.io/Haskell-Red-Black-Tree/
-- 1. No red node has a red parent or a red node has only black children.
-- 2. Every path from the root node to an empty node contains the same number of black nodes.
-- 3. The root and leaves of the tree are black.
import Data.Maybe (isNothing, fromJust)

data Color = Red | Black deriving (Show, Eq)

-- Is the derived Eq what we want? Maybe we want to ignore color?
data Tree a = Leaf | Node Color a (Tree a) (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node c x left right) = Node c (f x) (fmap f left) (fmap f right)

-- Now, Tree is an (endo)functor on the (+/-) category of Haskell types:
-- Associates each type with another type:
--   - Tree :: * -> *
--   - data Tree a = ...
-- Associates each function (morphism) with another function:
--   - fmap :: (a -> b) -> (Tree a - Tree b)

instance Foldable Tree where  -- we get folds, elem, length, ...
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf = mempty
  foldMap f (Node _ x left right) = foldMap f left <> f x <> foldMap f right

singleton :: a -> Tree a
singleton x = Node Black x Leaf Leaf

-- TODO: this does not care about the invariants yet
insert :: Ord a => a -> Tree a -> Tree a
insert v Leaf = singleton v
insert v (Node _ x left right)
  | v == x = Node Black x left right
  | v < x  = Node Black x (insert v left) right
  | v > x  = Node Black x left (insert v right)

fromFoldable :: (Foldable t, Ord a) => t a -> Tree a
fromFoldable = foldr insert Leaf

depth :: (Ord a) => a -> Tree a -> Maybe Int
depth _ Leaf = Nothing
depth v (Node _ x left right)
  | v == x = Just 0
  | v < x  = depth v left >>= (Just . (1+))
  | v > x  = depth v right >>= (Just . (1+))

maxDepth :: (Ord a) => Tree a -> Maybe Int
maxDepth Leaf = Nothing
maxDepth t = maxDepth' t
  where maxDepth' t = foldr (\x acc -> max acc (depth x t)) (Just 0) t


test :: Bool
test = and [singleton', insert', fromFoldable', depth', maxDepth']
  where
    singleton' = Node Black 0 Leaf Leaf == singleton 0
    insert' = Node Black 1 (Node Black 0 Leaf Leaf) Leaf == insert 0 (singleton 1)
    fromFoldable' = Node Black 2 (Node Black 1 Leaf Leaf) (Node Black 3 Leaf Leaf) == fromFoldable [1,3,2]
    depth' = notInTree && root && depth2
      where
        notInTree = isNothing $ depth 0 Leaf
        root = Just 0 == depth 0 (singleton 0)
        depth2 = Just 2 == depth 3 (fromFoldable [3,2,1])
    maxDepth' = Just 2 == maxDepth (fromFoldable [3,2,1])
