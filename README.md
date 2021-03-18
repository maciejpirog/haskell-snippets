# haskell-snippets
Pearly Haskell one-filers

- [Graph3Col](Graph3Col.hs) - A data structure that represents 3-colourable graphs. Not surprisingly, it is a graph together with a colouring, which is guaranteed to be correct thanks to type-level hackery. This file contains also a function that colours a graph using the most trivial backtracking algorithm. Thanks to the properties expressed in types, it is necessarily partially correct: if the function says a graph is 3-colourable, then it indeed is.

- [RotateList](RotateList.hs) - A lazy TABA (There And Back Again) list rotation. It is first-order, leaves no garbage behind, and requires only a single traversal of the list.

- [Tree23](Tree23.hs) - 2-3 trees together with insertion such that: we never construct a tree with broken invariant (even as a local temporary value), and insertions and rotations are defined in separate functions. This is surprisingly tricky, and the solution relies on heterogeneously typed smart constructors (via type classes).
