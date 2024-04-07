-- See: https://abhiroop.github.io/Haskell-Red-Black-Tree/

-- 1. No red node has a red parent or a red node has only black children.
-- 2. Every path from the root node to an empty node contains the same number of black nodes.
-- 3. The root and leaves of the tree are black.

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

blacken' :: Tree a -> Tree a
blacken' Leaf = Leaf
blacken' (Node _ x left right) = Node Black x left right

insert :: (Ord a) => a -> Tree a -> Tree a
insert x = blacken' . insert'
  where insert' Leaf = Node Red x Leaf Leaf
        insert' (Node c y left right)
          | x < y  = balance c y (insert' left) right
          | x == y = Node c y left right
          | x > y  = balance c y left (insert' right)

balance :: Color -> a -> Tree a -> Tree a -> Tree a
balance Black z (Node Red y (Node Red x a b) c) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black z (Node Red x a (Node Red y b c)) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red z (Node Red y b c) d) = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red y b (Node Red z c d)) = Node Red y (Node Black x a b) (Node Black z c d)
balance c x a b = Node c x a b

balance' :: Tree a -> Tree a
balance' (Node c v l r) = balance c v l r

-- TODO: delete stuff not in tree?
delete :: (Ord a) => a -> Tree a -> Tree a
-- delete = (blacken' .) . del
delete x t = blacken' $ del x t

del :: (Ord a) => a -> Tree a -> Tree a
del _ Leaf = Leaf
del x t@(Node _ y left right) = case compare x y of
  GT -> delRight x t
  LT -> delLeft x t
  EQ -> fuse left right

delLeft :: (Ord a) => a -> Tree a -> Tree a
delLeft x t@(Node Black y left right) = balLeft $ Node Black y (del x left) right
delLeft x t@(Node Red   y left right) = Node Red y (del x left) right

balLeft :: Tree a -> Tree a
balLeft (Node Black y (Node Red   x t1 t2) t3) = Node Red y (Node Black x t1 t2) t3
balLeft (Node Black y t1 (Node Black z t2 t3)) = balance' (Node Black y t1 (Node Red z t2 t3))
-- TODO: why is t4@(Node Black v l r) necessary? We don't refer to t4 ever
balLeft (Node Black y t1 (Node Red z (Node Black u t2 t3) t4@(Node Black v l r))) = Node Red u (Node Black y t1 t2) (balance' (Node Black z t3 (Node Red v l r)))

delRight :: (Ord a) => a -> Tree a -> Tree a
delRight x t@(Node Black y left right) = balRight $ Node Black y left (del x right)
delRight x t@(Node Red   y left right) = Node Red y left (del x right)

balRight :: Tree a -> Tree a
balRight (Node Black y t1 (Node Red   x t2 t3)) = Node Red y t1 (Node Black x t2 t3)
balRight (Node Black y (Node Black z t1 t2) t3) = balance' (Node Black y (Node Red z t1 t2) t3)
balRight (Node Black y (Node Red z t1@(Node Black v l r) (Node Black u t2 t3)) t4) = Node Red u (balance' (Node Black z (Node Red v l r) t2)) (Node Black y t3 t4)

fuse :: Tree a -> Tree a -> Tree a
fuse Leaf t = t
fuse t Leaf = t
fuse t1@(Node Black _ _ _) (Node Red y t3 t4) = Node Red y (fuse t1 t3) t4
fuse (Node Red x t1 t2) t3@(Node Black _ _ _) = Node Red x t1 (fuse t2 t3)
fuse (Node Red x t1 t2) (Node Red y t3 t4) =
  let s = fuse t2 t3 in case s of
    Leaf                 -> Node Red x t1 (Node Red y Leaf t4)
    (Node Red   z s1 s2) -> Node Red z (Node Red x t1 s1) (Node Red y s2 t4)
    (Node Black _  _  _) -> Node Red x t1 (Node Red y s t4)
fuse (Node Black x t1 t2) (Node Black y t3 t4) =
  let s = fuse t2 t3 in case s of
    Leaf                 -> balLeft (Node Black x t1 (Node Black y Leaf t4))
    (Node Red   z s1 s2) -> Node Red z (Node Black x t1 s1) (Node Black y s2 t4)
    (Node Black z s1 s2) -> balLeft (Node Black x t1 (Node Black y s t4))


fromFoldable :: (Foldable t, Ord a) => t a -> Tree a
fromFoldable = foldr insert Leaf

-- TODO: black depth
-- TODO: can I write a checkCondition-function?

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


-- TODO: conceptually simpler delete: https://matt.might.net/articles/red-black-delete/
-- TODO: testing: https://matt.might.net/articles/quick-quickcheck/


test :: Bool
test = and [singleton', insert', fromFoldable', depth', maxDepth', delete']
  where
    singleton' = Node Black 0 Leaf Leaf == singleton 0
    insert' = Node Black 1 (Node Red 0 Leaf Leaf) Leaf == insert 0 (singleton 1)
    fromFoldable' = Node Black 2 (Node Black 1 Leaf Leaf) (Node Black 3 Leaf Leaf) == fromFoldable [1,2,3]
    depth' = notInTree && root && depth1
      where
        notInTree = isNothing $ depth 0 Leaf
        root = Just 0 == depth 0 (singleton 0)
        depth1 = Just 1 == depth 3 (fromFoldable [3,2,1])
    maxDepth' = Just 8 == maxDepth (fromFoldable [1..2^8])  -- very cool: https://stackoverflow.com/a/6400628
    delete' = simpleDelete && deleteRight && deleteLeft
      where
        simpleDelete = Leaf == delete 1 (singleton 1)
        deleteRight = Node Black 2 (Node Red 1 Leaf Leaf) Leaf == delete 3 (fromFoldable [1,2,3])
        deleteLeft = Node Black 2 Leaf (Node Red 3 Leaf Leaf) == delete 1 (fromFoldable [1,2,3])
        deleteFuse = Node Black 2 (Node Black 1 Leaf Leaf) (Node Black 4 Leaf Leaf) == delete 3 (fromFoldable [1..4])
        -- TODO: delete 3 $ fromFoldable [1..4] reveals a forgotten pattern in fuse s,
        -- see also https://github.com/Abhiroop/okasaki/issues/2
