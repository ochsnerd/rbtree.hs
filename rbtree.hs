-- See: https://abhiroop.github.io/Haskell-Red-Black-Tree/

-- 1. No red node has a red parent or a red node has only black children.
-- 2. Every path from the root node to an empty node contains the same number of black nodes.
-- 3. The root and leaves of the tree are black.

-- TODO: can I write a checkCondition-function?
import Data.Maybe (isNothing, fromJust)

data Color = Red | Black deriving (Show, Eq)

-- Is the derived Eq what we want? Maybe we want to ignore color?
-- Leaves are implicitly black (see 3.)
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

insert :: Ord a => a -> Tree a -> Tree a
insert x = blacken . insert'
  where insert' Leaf = Node Red x Leaf Leaf
        insert' (Node c y left right)
          | x < y  = balance c y (insert' left) right
          | x == y = Node c y left right
          | x > y  = balance c y left (insert' right)
        blacken (Node _ y left right) = Node Black y left right

balance :: Color -> a -> Tree a -> Tree a -> Tree a
balance Black z (Node Red y (Node Red x a b) c) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black z (Node Red x a (Node Red y b c)) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red z (Node Red y b c) d) = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red y b (Node Red z c d)) = Node Red y (Node Black x a b) (Node Black z c d)
balance c x a b = Node c x a b

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
  where maxDepth' t = foldr (\x acc -> max acc (depth x t)) (Just 0) t -- can we be smarter?


-- TODO: delete seems to be a bit of a pain


test :: Bool
test = and [singleton', insert', fromFoldable', depth', maxDepth']
  where
    singleton' = Node Black 0 Leaf Leaf == singleton 0
    insert' = Node Black 1 (Node Red 0 Leaf Leaf) Leaf == insert 0 (singleton 1)
    fromFoldable' = Node Black 2 (Node Black 1 Leaf Leaf) (Node Black 3 Leaf Leaf) == fromFoldable [1,2,3]
    depth' = notInTree && root && depth1
      where
        notInTree = isNothing $ depth 0 Leaf
        root = Just 0 == depth 0 (singleton 0)
        depth1 = Just 1 == depth 3 (fromFoldable [3,2,1])
    maxDepth' = Just 16 == maxDepth (fromFoldable [1..2^16])  -- very cool: https://stackoverflow.com/a/6400628
