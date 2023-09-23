import Test.HUnit
import Prelude hiding (lookup)
import Lib as BST
import Dictionary (Dict, emptyDict, treeToDict, insertD, lookupD, removeD, listD)
import Test.QuickCheck
import Data.Maybe
import Data.List (sortBy)

instance (Arbitrary k, Arbitrary v) => Arbitrary (Tree k v) where
  arbitrary = sized arbTree
    where
      arbTree 0 = return Empty
      arbTree n = do
        left <- resize (n `div` 2) arbitrary
        right <- resize (n `div` 2) arbitrary
        k <- arbitrary
        v <- arbitrary
        return $ Node left k v right

-- Running the tests
main :: IO ()
main = do
    runTestTT allTests
    return()

allTests = TestList [treeTests, dictTests, TestLabel "Property tests: " (TestList propTests)]

propTests = [
    TestCase (putStrLn "\nCheck insertion size" >> quickCheck prop_insert_size),
    TestCase (putStrLn "\nCheck lookup is done properly" >> quickCheck prop_lookupInsertedKey),
    TestCase (putStrLn "\nCheck removing non-existing key does nothing" >> quickCheck prop_removeNonexistentKey),
    TestCase (putStrLn "\nCheck remove is done properly" >> quickCheck prop_removeRemovesKey),
    TestCase (putStrLn "\nCheck length for listEntries" >> quickCheck prop_sameLength),
    TestCase (putStrLn "\nCheck order for listEntries" >> quickCheck prop_orderedListEntries),
    TestCase (putStrLn "\nCheck rotate left and then right back to original" >> quickCheck prop_rotate)
  ]

treeTests :: Test
treeTests = TestList [testEmptyTree, treeInsertTests, treeLookupTests, treeRemoveTests, treeDisplayTests, treeRotationsTest]

testEmptyTree :: Test
testEmptyTree = TestCase (do
    let tree = (emptyTree :: Tree Int String)
    assertEqual "emptyTree should return an empty Tree" tree Empty)

treeInsertTests :: Test
treeInsertTests = TestList [testEmptyTreeInsertion, testGreaterTreeInsertion, testLesserTreeInsertion, testExistingKeyTreeInsertion, testExistingKeyTreeInsertionNegative]

testEmptyTreeInsertion :: Test
testEmptyTreeInsertion = TestCase (do
    let tree = (emptyTree :: Tree Int String)
        key = 5
        value = "five"
        expected = Node Empty key value Empty
    assertEqual "insert should add key-value pair to empty tree" expected (BST.insert key value tree))

testGreaterTreeInsertion :: Test
testGreaterTreeInsertion = TestCase (do
    let tree = Node Empty 5 "five" Empty
        key = 7
        value = "seven"
        expected = Node Empty 5 "five" (Node Empty 7 "seven" Empty)
    assertEqual "insert should add key-value pair to right subtree" expected (BST.insert key value tree))

testLesserTreeInsertion :: Test
testLesserTreeInsertion = TestCase (do
    let tree = Node Empty 5 "five" Empty
        key = 4
        value = "seven"
        expected = Node (Node Empty 4 "seven" Empty) 5 "five" Empty
    assertEqual "insert should add key-value pair to left subtree"  expected (BST.insert key value tree))

testExistingKeyTreeInsertion :: Test
testExistingKeyTreeInsertion = TestCase (do
    let tree = Node Empty 5 "five" Empty
        key = 5
        value = "newValue"
        expected = Node Empty key value Empty
    assertEqual "insert should replace existing keys value with newValue" expected (BST.insert key value tree))

testExistingKeyTreeInsertionNegative :: Test
testExistingKeyTreeInsertionNegative = TestCase (do
    let tree = Node Empty (-5) "five" Empty
        key = (-5)
        value = "newValue"
        expected = Node Empty key value Empty
    assertEqual "insert should replace existing keys value with newValue" expected (BST.insert key value tree))

--Property-Based Tests
prop_insert_size :: Property
prop_insert_size = forAll (genUniqueKVs []) (\kvs ->
  let t = foldr (uncurry insert) Empty (tail kvs)
      (k, v) = head kvs                         
      t' = insert k v t
  in size t' === size t + 1)

treeLookupTests :: Test
treeLookupTests = TestList [testEmptyTreeLookup, testGreaterTreeLookup, testLesserTreeLookup, testMissingLookup]

testEmptyTreeLookup :: Test
testEmptyTreeLookup = TestCase (do
    let tree = (emptyTree :: Tree Int String)
        key = 5
    assertEqual "Lookup should find Nothing" Nothing (BST.lookup key tree))

testGreaterTreeLookup :: Test
testGreaterTreeLookup = TestCase (do
    let tree = Node Empty 8 "eight" (Node Empty 9 "nine" Empty)
        key = 9
        expected = Just "nine"
    assertEqual "lookup should find right sub-tree" expected (BST.lookup key tree))

testLesserTreeLookup :: Test
testLesserTreeLookup = TestCase (do
    let tree = Node (Node Empty 6 "six" Empty) 8 "eight" Empty
        key = 6
        expected = Just "six"
    assertEqual "lookup should find left sub-tree" expected (BST.lookup key tree))

testMissingLookup :: Test
testMissingLookup = TestCase (do
    let tree = Node (Node Empty 6 "six" Empty) 8 "eight" Empty
        key = 5
        expected = Nothing
    assertEqual "lookup should find Nothing" expected (BST.lookup key tree))

--Property-Based Tests
prop_lookupInsertedKey :: Int -> String -> Tree Int String -> Bool
prop_lookupInsertedKey k v tree =
  let newTree = BST.insert k v tree
  in
    if BST.lookup k newTree == Just v
      then True
      else False

treeRemoveTests :: Test
treeRemoveTests = TestList [testEmptyTreeRemove, testGreaterTreeRemove, testRootTreeRemove, testRightmostTreeRemove, testMissingRemove]

treeRemove :: Tree Int String
treeRemove = BST.insert 15 "fifteen" (BST.insert 10 "ten" (BST.insert 20 "twenty" (BST.insert 18 "eighteen" (BST.insert 30 "thirty" (BST.insert 16 "sixteen" (BST.insert 19 "nineteen" Empty))))))

testEmptyTreeRemove :: Test
testEmptyTreeRemove = TestCase (do
    let tree = (emptyTree :: Tree Int String)
        key = 5
        expected = Empty
    assertEqual "remove should do Nothing" expected (BST.remove key tree))

testGreaterTreeRemove :: Test
testGreaterTreeRemove = TestCase (do
    let tree = treeRemove
        key = 20
        expected = Node (Node (Node Empty 10 "ten" (Node Empty 15 "fifteen" Empty)) 16 "sixteen" (Node Empty 18 "eighteen" Empty)) 19 "nineteen" (Node Empty 30 "thirty" Empty)
    assertEqual "remove should delete right sub-tree node" expected (BST.remove key tree))

testRootTreeRemove :: Test
testRootTreeRemove = TestCase (do
    let tree = treeRemove
        key = 15
        expected = Node (Node (Node Empty 10 "ten" Empty) 16 "sixteen" (Node Empty 18 "eighteen" Empty)) 19 "nineteen" (Node (Node Empty 20 "twenty" Empty) 30 "thirty" Empty)
    assertEqual "remove should delete root node" expected (BST.remove key tree))

testRightmostTreeRemove :: Test
testRightmostTreeRemove = TestCase (do
    let tree = treeRemove
        key = 30
        expected = Node (Node (Node Empty 10 "ten" (Node Empty 15 "fifteen" Empty)) 16 "sixteen" (Node Empty 18 "eighteen" Empty)) 19 "nineteen" (Node Empty 20 "twenty" Empty)
    assertEqual "remove should remove rightmost node" expected (BST.remove key tree))

testMissingRemove :: Test
testMissingRemove = TestCase (do
    let tree = treeRemove
        key = 7
        expected = Node (Node (Node Empty 10 "ten" (Node Empty 15 "fifteen" Empty)) 16 "sixteen" (Node Empty 18 "eighteen" Empty)) 19 "nineteen" (Node (Node Empty 20 "twenty" Empty) 30 "thirty" Empty)
    assertEqual "remove should remove nothing" expected (BST.remove key tree))

--Property-Based Tests
genUniqueKVs :: [(Int, String)] -> Gen [(Int, String)]
genUniqueKVs existingKVs = do
  k <- arbitrary `suchThat` (\x -> not (elem x (map fst existingKVs)))
  v <- arbitrary
  return ((k, v) : existingKVs)

prop_removeRemovesKey :: Int -> Property
prop_removeRemovesKey k =
  forAll genUniqueKVsTree (\t ->
    if isJust (BST.lookup k t)
      then let newTree = BST.remove k t
           in isNothing (BST.lookup k newTree)
      else True)

prop_removeNonexistentKey :: Int -> Tree Int String -> Property
prop_removeNonexistentKey k tree =
  isNothing (BST.lookup k tree) ==>
    let newTree = BST.remove k tree
    in newTree == tree

treeDisplay :: Tree Int String
treeDisplay = BST.insert 15 "fifteen" (BST.insert 10 "ten" (BST.insert 20 "twenty" (BST.insert 18 "eighteen" (BST.insert 30 "thirty" (BST.insert 16 "sixteen" (BST.insert 19 "nineteen" Empty))))))

treeDisplayTests :: Test
treeDisplayTests = TestList [testEmptyTreeDisplay, testTreeDisplay]

testEmptyTreeDisplay :: Test
testEmptyTreeDisplay = TestCase (do
    let tree = (emptyTree :: Tree Int String)
        expected = []
    assertEqual "Display should display Nothing" (Just expected) (Just (BST.listEntries tree)))

testTreeDisplay :: Test
testTreeDisplay = TestCase (do
    let tree = treeDisplay
        expected = [(10,"ten"),(15,"fifteen"),(16,"sixteen"),(18,"eighteen"),(19,"nineteen"),(20,"twenty"),(30,"thirty")]
    assertEqual "Display should display keys and value in order" expected (BST.listEntries tree))

--Property-Based Tests
prop_orderedListEntries :: Property
prop_orderedListEntries = 
  forAll genUniqueKVsTree (\t -> 
    collect (size t) (
    let entries = listEntries t
        keys = map fst entries
    in ordered keys === True))
  where
    ordered [] = True
    ordered [_] = True
    ordered (k:v:kv) = k <= v && ordered (v:kv)

prop_sameLength :: Tree Int String -> Bool
prop_sameLength tree =
  let entries = listEntries tree
  in length entries == size tree

genUniqueKVsTree :: Gen (Tree Int String)
genUniqueKVsTree = do
  kvs <- genUniqueKVs []
  return (foldr (uncurry BST.insert) Empty kvs)
  
treeRotationsTest :: Test
treeRotationsTest = TestList [testRotateRight, testRotateLeft]

treeRotate = Node (Node (Node Empty 1 "one" Empty) 2 "two" (Node Empty 3 "three" Empty)) 4 "four" (Node (Node Empty 5 "five" Empty) 6 "six" (Node Empty 7 "seven" Empty))

testRotateRight :: Test
testRotateRight = TestCase (do
    let tree = treeRotate
        expected = Node (Node Empty 1 "one" Empty) 2 "two" (Node (Node Empty 3 "three" Empty) 4 "four" (Node (Node Empty 5 "five" Empty) 6 "six" (Node Empty 7 "seven" Empty)))
    assertEqual "rotateRight Should rotate Right" expected (BST.rotate False tree)) 

testRotateLeft :: Test
testRotateLeft = TestCase (do
    let tree = treeRotate
        expected = Node (Node (Node (Node Empty 1 "one" Empty) 2 "two" (Node Empty 3 "three" Empty)) 4 "four" (Node Empty 5 "five" Empty)) 6 "six" (Node Empty 7 "seven" Empty)
    assertEqual "rotateLeft Should rotate Left" expected (BST.rotate True tree))

--Property-Based Tests
prop_rotate :: Bool -> Tree Int Char -> Bool
prop_rotate b t = t == (rotate b (rotate (not b) t))

dictTests :: Test
dictTests = TestList [testEmptyDict, testLookupDict, testInsertDict, testRemoveDict, testListDict]

testEmptyDict :: Test
testEmptyDict = TestCase (do
    let dict = emptyDict :: Dict Int String
    assertEqual "emptyDictionary should return an empty dictionary" dict emptyDict)

tree1 :: Tree Int String
tree1 = Node Empty 1 "one" (Node Empty 3 "three" Empty)

dict1 :: Dict Int String
dict1 = treeToDict tree1

testInsertDict :: Test
testInsertDict = TestCase (do
    let dict = emptyDict
        dict' = insertD 1 "one" dict
        dict'' = insertD 3 "three" dict'
        expected = dict1
    assertEqual "insertD should add a new pair to the dictionary" expected dict'')

testLookupDict :: Test
testLookupDict = TestCase (do
    let expected = Just "one"
    assertEqual "lookupD should find a value according to a key in the dictionary" expected (lookupD 1 dict1))

testRemoveDict :: Test
testRemoveDict = TestCase (do
    let dict' = removeD 3 dict1
        tree' = Node Empty 1 "one" Empty
        expected = treeToDict tree'
    assertEqual "removeD should remove a pair from the dictionary according to the key provided" expected dict')

testListDict :: Test
testListDict = TestCase (do
    let expected = [(1,"one"),(3,"three")]
    assertEqual "listD should make dictionary into list" expected (listD dict1))