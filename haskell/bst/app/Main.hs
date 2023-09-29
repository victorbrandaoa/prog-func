module Main (main) where

import Lib

exampleTree :: BinaryTree Int
exampleTree =
  Node 5
    (Node 3 (Node 1 NIL NIL) (Node 4 NIL NIL))
    (Node 7 (Node 6 NIL NIL) (Node 9 NIL NIL))

main :: IO ()
main = do
  let t = exampleTree
  print (isBST t)
