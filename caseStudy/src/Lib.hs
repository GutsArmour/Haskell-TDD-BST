{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib where
import Prelude hiding (lookup)

data Tree k v = Empty | Node (Tree k v) k v (Tree k v) deriving (Show, Read, Eq)

emptyTree :: Tree k v
emptyTree = Empty

insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert k v Empty = Node Empty k v Empty
insert k v (Node left key val right) =
  if k == key
    then Node left key v right
    else if k < key
      then Node (insert k v left) key val right
      else Node left key val (insert k v right)

lookup :: Ord k => k -> Tree k v -> Maybe v
lookup _ Empty = Nothing
lookup k (Node left key val right) =
  if k == key
    then Just val
    else if k < key
      then lookup k left
      else lookup k right

remove :: Ord k => k -> Tree k v -> Tree k v
remove _ Empty = Empty
remove k (Node left key val right) =
  if k == key
    then removeNode (Node left key val right)
    else if k < key
      then Node (remove k left) key val right
    else Node left key val (remove k right)
  where 
    removeNode :: Ord k => Tree k v -> Tree k v
    removeNode (Node Empty _ _ right) = right
    removeNode (Node left _ _ Empty) = left
    removeNode (Node left _ _ right) =
      let (key', val', right') = findLeftmost right
      in Node left key' val' (remove key' right')
    findLeftmost :: Tree k v -> (k, v, Tree k v)
    findLeftmost (Node Empty key val right) = (key, val, right)
    findLeftmost (Node left key val right)=
      let (key', val', left') = findLeftmost left
      in (key', val', Node left' key val right)

listEntries :: Tree k v -> [(k, v)]
listEntries Empty = []
listEntries (Node left k v right) =
  listEntries left ++ [(k, v)] ++ listEntries right

displayEntries :: (Show k, Show v) => Tree k v -> IO ()
displayEntries tree = print (listEntries tree)

rotate :: Bool -> Tree k v -> Tree k v
rotate True (Node leftSubtree k1 v1 (Node leftSubtreeRight k2 v2 rightSubtree)) =
  Node (Node leftSubtree k1 v1 leftSubtreeRight) k2 v2 rightSubtree
rotate False (Node (Node leftSubtreeLeft k1 v1 rightSubtreeLeft) k2 v2 rightSubtree) =
  Node leftSubtreeLeft k1 v1 (Node rightSubtreeLeft k2 v2 rightSubtree)
rotate _ tree = tree

size :: Tree k v -> Int
size Empty = 0
size (Node left _ _ right) = 1 + size left + size right

keys :: Tree k v -> [k]
keys Empty = []
keys (Node left k _ right) = keys left ++ [k] ++ keys right