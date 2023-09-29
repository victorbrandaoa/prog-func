module Lib
  ( BinaryTree(..),
    isBST,
    mirror
  ) where

import Data.List (intercalate)

data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

leaves (Node a NIL NIL) = [a]
leaves (Node a left right) = leaves left ++ leaves right

elementsAtLevel NIL 0 = []
elementsAtLevel (Node a _ _) 0 = [a]
elementsAtLevel (Node a NIL right) k = elementsAtLevel right (k - 1)
elementsAtLevel (Node a left NIL) k = elementsAtLevel left (k - 1)
elementsAtLevel (Node a left right) k = elementsAtLevel left (k - 1) ++ elementsAtLevel right (k - 1)

mirror NIL = NIL
mirror (Node a NIL NIL) = (Node a NIL NIL)
mirror (Node a left right) = (Node a (mirror right) (mirror left))

mapTree _ NIL = NIL
mapTree f (Node a left right) = (Node (f a) (mapTree f left) (mapTree f right))

height NIL = -1
height (Node a left right) = 1 + (max (height left) (height right))

sizeBT NIL = 0
sizeBT (Node a left right) = 1 + sizeBT left + sizeBT right

maximumBT (Node a NIL NIL) = a
maximumBT (Node a left NIL) = max a (maximumBT left)
maximumBT (Node a NIL right) = max a (maximumBT right)
maximumBT (Node a left right) = max a (max (maximumBT left) (maximumBT right))

minimumBT (Node a NIL NIL) = a
minimumBT (Node a left NIL) = min a (minimumBT left)
minimumBT (Node a NIL right) = min a (minimumBT right)
minimumBT (Node a left right) = min a (min (minimumBT left) (minimumBT right))

isBST NIL = False
isBST (Node a NIL NIL) = True
isBST (Node a left right) | a < (maximumBT left) || a > (minimumBT right) = False
                          | otherwise = (isBST left) && (isBST right)

insertBST NIL v = (Node v NIL NIL)
insertBST (Node a left right) v | a > v = (Node a (insertBST left v) right)
                                | a < v = (Node a left (insertBST right v))
                                | otherwise = (Node a left right)

getNodeValue (Node a _ _) = a

getLeftBST (Node a left _) = left

getRightBST (Node a _ right) = right

searchBST NIL _ = NIL
searchBST (Node a left right) e | a == e = (Node a left right)
                                | e > a = searchBST right e
                                | e < a = searchBST left e

order (Node a NIL NIL) = [a]
order (Node a NIL right) = [a] ++ order right
order (Node a left NIL) = order left ++ [a]
order (Node a left right) = order left ++ [a] ++ order right

preorder (Node a NIL NIL) = [a]
preorder (Node a NIL right) = [a] ++ preorder right
preorder (Node a left NIL) = [a] ++ preorder left
preorder (Node a left right) = [a] ++ preorder left ++ preorder right

postorder (Node a NIL NIL) = [a]
postorder (Node a NIL right) = postorder right ++ [a]
postorder (Node a left NIL) = postorder left ++ [a]
postorder (Node a left right) = postorder left ++ postorder right ++ [a]

remove NIL _ = NIL
remove (Node a left right) v
  | v < a = Node a (remove left v) right
  | v > a = Node a left (remove right v)
  | otherwise = case right of
      NIL -> left
      _   -> Node minValue left (remove right minValue)
    where minValue = minimumBT right
