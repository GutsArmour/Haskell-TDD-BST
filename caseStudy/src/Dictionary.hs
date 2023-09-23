module Dictionary (
    Dict,
    emptyDict,
    treeToDict,
    insertD,
    lookupD,
    removeD,
    listD
) where

import Prelude hiding (lookup)

import Lib as BST

newtype Dict k v = Dict (BST.Tree k v) deriving (Eq, Show)

emptyDict :: Dict k v
emptyDict = Dict BST.emptyTree

treeToDict :: BST.Tree k v -> Dict k v
treeToDict t = Dict t

insertD :: Ord k => k -> v -> Dict k v -> Dict k v
insertD k v (Dict t) = Dict (BST.insert k v t)

lookupD :: Ord k => k -> Dict k v -> Maybe v
lookupD k (Dict t) = BST.lookup k t

removeD :: Ord k => k -> Dict k v -> Dict k v
removeD k (Dict t) = Dict (BST.remove k t)

listD :: Dict k v -> [(k,v)]
listD (Dict t) = BST.listEntries t