# haskell-snippets
Pearly Haskell one-filers

- [Graph3Col](Graph3Col.hs) - A data structure that represents 3-colourable graphs. Not surprisingly, it is a graph together with a colouring, which is guaranteed to be correct thanks to type-level hackery. This file contains also a function that colours a graph using the most trivial backtrcking algorithm. Thanks to the properties expressed in types, it is necessarily partially correct: if the function says a graph is 3-colourable, then it indeed is.
