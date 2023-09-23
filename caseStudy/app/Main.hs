module Main (main) where

import Lib

main :: IO ()
main = do 
    let treeDisplay :: Tree Int String
        treeDisplay = insert 15 "fifteen" (insert 10 "ten" (insert 20 "twenty" (insert 18 "eighteen" (insert 30 "thirty" (insert 16 "sixteen" (insert 19 "nineteen" Empty)))))) 
    displayEntries treeDisplay


