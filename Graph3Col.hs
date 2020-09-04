-- 2020 Maciej Piróg
-- License: MIT
--
-- A data structure that represents 3-colourable graphs.
--
-- Not surprisingly, it is a graph together with a colouring, which is
-- guaranteed to be correct thanks to type-level hackery. This file
-- contains also a function that colours a graph using the most
-- trivial backtrcking algorithm. Thanks to the properties expressed
-- in types, it is partially correct: if the function says a graph is
-- 3-colourable, then it indeed is.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Graph3Col where

import GHC.TypeLits (TypeError, ErrorMessage(Text))
import Data.Maybe (listToMaybe)

------------
-- GRAPHS --
------------

-- We start by defining three colours used at the type level, and
-- singletons that represent them on the value level.

data Color = R | G | B

data SColor :: Color -> * where
  Red   :: SColor R
  Green :: SColor G
  Blue  :: SColor B

-- Two colours are different if they are not the same. We use a custom
-- type error message to let the user know why their graph won't
-- type-check.

type family DifferentColor (c :: Color) (d :: Color) :: Bool where
  DifferentColor c c = TypeError (Text "Illegal edge")
  DifferentColor _ _ = True

-- A 3-colourable graph is a list of vertices, where each vertex is a
-- colour together with a list of Yes's and No's that indicate if
-- there is an edge to a given previous vertex. The invariants in
-- types guarantee that the list for each vertex is of appropriate
-- length and that it is always a No for two vertices of the same
-- colour. The Edges datatype is a list of coincidence of a vertex. It
-- is indexed by the colour of the current vertex and a list of
-- colours of previous vertices.

data Edges :: Color -> [Color] -> * where
  Yes  :: (DifferentColor c x ~ True) => Edges c xs -> Edges c (x : xs)
  No   :: Edges c xs -> Edges c (x : xs)
  Done :: Edges c '[]

-- An auxiliary datatype that stores vertices. It is indexed by the
-- list of colours of all vertices in the graph.

data CGraph :: [Color] -> * where
  Vertex :: SColor c -> Edges c cs -> CGraph cs -> CGraph (c : cs) 
  Empty :: CGraph '[]

-- The graph itself is CGraph with its index hidden behind an
-- existential quantifier.

data Graph3Col = forall n. Graph3Col (CGraph n)

--------------
-- EXAMPLES --
--------------

(a,b) <#> c = Vertex a b c
infixr <#>

{-
       0--1               R--B
Graph  |\ |  coloured as  |\ |
       | \|               | \|
       2--3               B--G
-}

ex1 :: Graph3Col
ex1 = Graph3Col $
  (Green, Yes $ Yes $ Yes $ Done) <#>  -- 3
  (Blue,  No  $ Yes $ Done)       <#>  -- 2
  (Blue,  Yes $ Done)             <#>  -- 1
  (Red,   Done)                   <#>  -- 0
  Empty

-----------------------
-- COLOURING A GRAPH --
-----------------------

-- We represent a non-coloured graph as a triangle of coincidence
-- (just like in CGraph, but without the fancy indexing).

newtype Graph = Graph [[Bool]]

-- IsColorDifferent tells us if two colours are not the
-- same. Additionally, each constructor is equipped with a witness,
-- that is, it introduces a constraint, either (DifferentColor c d ~
-- True) or (c ~ d).

data IsColorDifferent :: Color -> Color -> * where
  Different :: (DifferentColor c d ~ True) => IsColorDifferent c d
  TheSame   :: (c ~ d)                     => IsColorDifferent c d

-- The compareColor function tells us if two colours are the same. We
-- cannot use eq or even wildcards in the pattern, because only by
-- matching a pattern, we introduce an appropriate constraint needed
-- by the constructor of IsColorDifferent.

compareColor :: SColor c -> SColor d -> IsColorDifferent c d
compareColor Red   Red   = TheSame
compareColor Blue  Blue  = TheSame
compareColor Green Green = TheSame
compareColor Red   Green = Different
compareColor Red   Blue  = Different
compareColor Green Red   = Different
compareColor Green Blue  = Different
compareColor Blue  Red   = Different
compareColor Blue  Green = Different

-- The toEdges function tries to add a new vertex (specified by a
-- colour and a coincidence list) to an existing CGraph. It returns
-- either an empty list or a singleton of Edges that you can create
-- this way. Note that we answer with an Edges and not a CGraph,
-- because we need its index in the addVertex function.

toEdges :: SColor c -> [Bool] -> CGraph cs -> [Edges c cs]
toEdges c []           Empty          = [Done]
toEdges c (False : bs) (Vertex d _ g) = No <$> toEdges c bs g
toEdges c (True  : bs) (Vertex d _ g) = case compareColor c d of
  Different -> Yes <$> toEdges c bs g
  TheSame   -> []
toEdges _ _            _              = []

-- The addVertex function tires to ass a new vertex specified only by
-- a coincidence list. I tries all three colours.

addVertex :: [Bool] -> Graph3Col -> [Graph3Col]
addVertex bs (Graph3Col g) = aux Red ++ aux Green ++ aux Blue
 where
  aux :: SColor a -> [Graph3Col]
  aux c = do
   e <- toEdges c bs g
   return $ Graph3Col $ Vertex c e g

-- Exercise for the reader: Why can't we define addVertex as follows?
--
-- addVertex bs (Graph3Col g) = do
-- c <- [Red, Green, Blue]
-- e <- toEdges c bs g
-- return $ Graph3Col $ Vertex c e g

-- The allColorings function tries to add all vertices from the
-- non-coloured graph one by one. This results in a list of all
-- possible colourings.

allColorings :: Graph -> [Graph3Col]
allColorings (Graph []) = [Graph3Col Empty]
allColorings (Graph (bs:ps)) = allColorings (Graph ps) >>= addVertex bs

-- Finally, to say if a graph is colourable, we try to extract the
-- head of the list of all possible colourings.

colorGraph :: Graph -> Maybe Graph3Col
colorGraph = listToMaybe . allColorings

--------------
-- EXAMPLES --
--------------

deriving instance Show (SColor c)
deriving instance Show (Edges c cs)
deriving instance Show (CGraph cs)
deriving instance Show (Graph3Col)

{-
       0--1
Graph  |\ |
       | \|
       2--3
-}
gex1 :: Graph
gex1 = Graph
  [[True, True, True],
   [False, True],
   [True],
   []]

{-
       0--1
Graph  |\/|
       |/\|
       2--3
-}
gex2 :: Graph
gex2 = Graph
  [[True, True, True],
   [True, True],
   [True],
   []]

{-
>>> colorGraph gex1
Just (Graph3Col
  (Vertex Blue  (Yes (Yes (Yes Done)))
  (Vertex Green (No  (Yes Done))
  (Vertex Green (Yes Done)
  (Vertex Red Done
  Empty)))))
>>> colorGraph gex2
Nothing
-}
