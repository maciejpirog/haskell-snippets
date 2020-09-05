{- 2020 Maciej Piróg

This file defines 2-3 trees together with insertion such that:

- We never construct a tree with broken invariant (even as a local
  temporary value),

- Insertions and rotations are defined in separate functions.

This is surprisingly tricky, and the solution relies on heterogenously
typed smart constructors (via type classes).
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Tree23 where

data Tree23 a = Node2 (Tree23 a) a (Tree23 a)
              | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
              | Empty23
  deriving (Show)

data InsResult a = Grown (Tree23 a) a (Tree23 a) | Balanced (Tree23 a)

-- -----------
-- Scaffolding
-- -----------

-- Each smart constructor takes arguments that are instances of the
-- ExtendsTree23 class, which tells if a given value represents a
-- balanced 2-3 tree or something that becomes such a tree after a
-- rotation.

class ExtendsTree23 t where
  toTree :: t x -> Maybe (Tree23 x)

instance ExtendsTree23 Tree23 where
  toTree = Just

instance ExtendsTree23 InsResult where
  toTree (Balanced t) = Just t
  toTree _            = Nothing

-- Each smart constructor (node2, node3) calls the appropriate rotate
-- function only if at least one (in practice: exactly one) argument
-- is not balanced.

class (ExtendsTree23 a, ExtendsTree23 b) => CNode2 a b where
  rotate2 :: Ord x => a x -> x -> b x -> InsResult x
  node2   :: Ord x => a x -> x -> b x -> InsResult x
  node2 (toTree -> Just l) x (toTree -> Just r) = Balanced $ Node2 l x r
  node2 l x r = rotate2 l x r

class (ExtendsTree23 a, ExtendsTree23 b, ExtendsTree23 c) => CNode3 a b c where
  rotate3 :: Ord x => a x -> x -> b x -> x -> c x -> InsResult x
  node3   :: Ord x => a x -> x -> b x -> x -> c x -> InsResult x
  node3 (toTree -> Just l) x (toTree -> Just m) y (toTree -> Just r) =
    Balanced $ Node3 l x m y r
  node3 l x m y r = rotate3 l x m y r

-- ---------
-- Rotations
-- ---------

-- We define rotations by instantiating the appropriate type classes,
-- based on the subtree in which we recursively insert the element. Of
-- course we can assume that the constructor of insResult is Grown, as
-- we handle the other case generically in the default definition of
-- the smart constructor (node2, node3).

instance CNode2 InsResult Tree23 where
  rotate2 (Grown u a v) b w = Balanced $ Node3 u a v b w

instance CNode2 Tree23 InsResult where
  rotate2 u a (Grown v b w) = Balanced $ Node3 u a v b w

instance CNode3 InsResult Tree23 Tree23 where
  rotate3 (Grown u a v) b w c x = Grown (Node2 u a v) b (Node2 w c x)

instance CNode3 Tree23 InsResult Tree23 where
  rotate3 u a (Grown v b w) c x = Grown (Node2 u a v) b (Node2 w c x)

instance CNode3 Tree23 Tree23 InsResult where
  rotate3 u a v b (Grown w c x) = Grown (Node2 u a v) b (Node2 w c x)

-- ---------
-- Insertion
-- ---------

-- Now the insertion looks as if we were inserting to a non-balanced
-- 2-3 tree: the rotations happen behind the scenes via the smart
-- constructors, which, if necessary, rotate the tree.

ins :: Ord a => a -> Tree23 a -> InsResult a
ins a Empty23                       = Grown Empty23 a Empty23
ins a (Node2 l b r)     | a <= b    = node2 (ins a l) b r
                        | otherwise = node2 l b (ins a r)
ins a (Node3 l b m c r) | a <= b    = node3 (ins a l) b m c r
                        | a >  c    = node3 l b m c (ins a r)
                        | otherwise = node3 l b (ins a m) c r

insert :: Ord a => a -> Tree23 a -> Tree23 a
insert a t = case ins a t of Balanced t  -> t
                             Grown l a r -> Node2 l a r
