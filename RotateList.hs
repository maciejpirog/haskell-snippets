{-
Maciej Piróg
maciej.adam.pirog@gmail.com

A lazy TABA (There And Back Again) list rotation.

It is first-order, leaves no garbage behind, and requires only a
single traversal of the list.

Going "there" calculates the length of the list (k), while going
"back" gathers the suffix of the original list that becomes the prefix
of the rotated list (ss), and, when it's done, gathers the rest of the
list to be the suffix of the rotated list (ps). Thanks to laziness,
we're allowed to build ss before we can start calculating its tail,
ps.
-}

module RotateList where

rotateRight :: Int -> [a] -> [a]
rotateRight m xs = ss
 where
  (_, ps, ss)  = aux xs 0
  aux (x:xs) k = case aux xs (k+1) of
                   (0, ps, ss) -> (0,   x:ps,   ss)
                   (n, ps, ss) -> (n-1,   ps, x:ss)
  aux []     k = (mod m k, [], ps)
