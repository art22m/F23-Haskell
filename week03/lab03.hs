{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Data.Text (Text)

-- 1.1 VISUALISING LISTS

-- | Sample list with 'Text' values.
sampleList :: [Text]
sampleList = ["a", "b", "c", "d", "e", "f", "g"]

samplePictureList :: [Picture]
samplePictureList = listTextToListPictures sampleList

-- | Draw a picture in a 1x1 square.
inSquare :: Picture -> Picture
inSquare p = p <> rectangle 1 1 <> colored white (solidRectangle 1 1)

inCircle :: Picture -> Picture
inCircle p = circle 0.5 <> p

-- Draw a list of values.
drawList :: [Picture] -> Picture
drawList (x:xs) = x <> polyline [(0, 0), (1.5, 0)] <> translated 2 0 (drawList xs)
drawList [] = solidCircle 0.4

-- Maps Text to Picture in square
textToPicture :: Text -> Picture
textToPicture txt = inSquare $ lettering txt

textToPictureCircle :: Text -> Picture
textToPictureCircle txt = inCircle $ lettering txt

-- Maps list of Text to list of Picture
listTextToListPictures :: [Text] -> [Picture]
listTextToListPictures lst = map textToPicture lst

-- Simple linked list
example1 :: IO ()
example1 = drawingOf $ drawList $ samplePictureList


-- 1.2 TREES

-- 1.2.1 Introducing binary trees

-- | A binary tree with values at leaves.
data Tree a 
  = Empty -- ˆ An empty tree.
  | Leaf a -- ˆ A leaf with a single value.
  | Node (Tree a) (Tree a) -- ˆ A node with two subtrees.

-- | A sample tree.
sampleTree :: Tree Text
sampleTree =
  Node
    (Node (Leaf "1") (Node Empty (Leaf "3")))
    (Node
      (Node (Node (Leaf "4") (Leaf "5")) Empty)
      (Node (Leaf "7") (Leaf "8")))


-- | Height of a tree.
height :: Tree a -> Int
height Empty = 0
height (Leaf _) = 1
height (Node l r) = 1 + max (height l) (height r)

-- | Count nodes.
countNodes :: Tree a -> Int
countNodes (Node l r) = 1 + countNodes l + countNodes r
countNodes _ = 0

-- 1.2.2 Visualising a tree

-- | Draw a tree with leaves at the same level.
drawTree :: Tree Picture -> Picture
drawTree Empty = blank
drawTree (Leaf p) = inSquare p
drawTree (Node l r) = node <> leftSubtree <> leftBranch l <> rightSubtree <> rightBranch r
  where
    node = solidCircle 0.2
    leftHeight = height l
    rightHeight = height r
    
    leftDx = fromIntegral $ -1 - countNodes r
    leftDy = fromIntegral $ -1 - rightHeight + min leftHeight rightHeight
    leftBranch Empty = blank
    leftBranch _ = polyline [(0, 0), (leftDx, leftDy)]
    leftSubtree = translated leftDx leftDy (drawTree l)
    
    rightDx = fromIntegral $ 1 + countNodes l
    rightDy = fromIntegral $ -1 - leftHeight + min leftHeight rightHeight
    rightBranch Empty = blank
    rightBranch _ = polyline [(0, 0), (rightDx, rightDy)]
    rightSubtree = translated rightDx rightDy (drawTree r)

-- | A sample tree with 'Picture's.
samplePictureTree :: Tree Picture
samplePictureTree =
  Node
        (Node (leaf "1") (Node Empty (leaf "3")))
        (Node
           (Node (Node (leaf "4") (leaf "5")) Empty)
           (Node (leaf "7") (leaf "8"))
        )
  where
    leaf s = Leaf (inSquare (lettering s))

-- Simple tree
example2 :: IO()
example2 = drawingOf $ drawTree samplePictureTree

-- 1.2.3 Mapping over leaves

-- | Apply a function to every leaf in a tree.
mapLeaves :: (a -> b) -> Tree a -> Tree b
mapLeaves _ Empty = Empty
mapLeaves g (Leaf p) = Leaf (g p)
mapLeaves g (Node l r) = Node (mapLeaves g l) (mapLeaves g r) 

example3 :: IO()
example3 = drawingOf $ drawTree (mapLeaves textToPicture sampleTree)

-- 1.2.4 Trees and lists

-- | Collect leaf values of a tree.
treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Leaf p) = [p]
treeToList (Node l r) = treeToList l ++ treeToList r 

example4 :: IO()
example4 = drawingOf $ drawList $ treeToList samplePictureTree

-- | Construct a tree from a list of values.
-- NOTE: this is degradate tree :)
listToTree :: [a] -> Tree a 
listToTree [] = Empty
listToTree (x:xs) = Node (Leaf x) (listToTree xs)

example5 :: IO()
example5 = drawingOf $ drawTree $ listToTree samplePictureList

-- 1.2.5 Balanced tree

-- | Construct a balanced tree from a list of leaves.
balancedTree :: [a] -> Tree a
balancedTree xs = merge (map Leaf xs)
  where
    merge :: [Tree a] -> Tree a
    merge [] = Empty
    merge [t] = t
    merge ts = merge (pairs ts)
  
    pairs :: [Tree a] -> [Tree a]
    pairs (l:r:ts) = Node l r : pairs ts
    pairs ts = ts
    
example6 :: IO()
example6 = drawingOf $ drawTree $ balancedTree samplePictureList

-- 1.2.6 Labelled nodes

-- | A binary tree with labelled nodes.
-- Leaves contain values of type @a@.
-- Nodes contain values (labels) of type @n@.
data LTree n a
  = LEmpty                     
  | LLeaf a                    
  | LNode n (LTree n a) (LTree n a) 

-- | Draw a tree with leaves at the same level.
drawLTree :: LTree Picture Picture -> Picture 
drawLTree LEmpty = blank
drawLTree (LLeaf p) = p
drawLTree (LNode p l r) = _


-- | Remove labels from the nodes of a tree.
removeLabels :: LTree n a -> Tree a
removeLabels LEmpty = Empty
removeLabels (LLeaf p) = Leaf p
removeLabels (LNode p l r) = Node (removeLabels l) (removeLabels r)

-- | Sample tree with 'Text'-labelled nodes and 'Text' values.
sampleLTree :: LTree Text Text
sampleLTree =
  LNode "*"
    (LNode "+"
      (LLeaf "1")
        (LNode "f" LEmpty (LLeaf "3")))
        (LNode "/"
      (LNode "+"
        (LNode "–" (LLeaf "4") (LLeaf "5"))
        LEmpty)
      (LNode "g"
        (LLeaf "7")
        (LLeaf "8")))

ltreeTextToLtreePictures :: LTree Text Text -> LTree Picture Picture
ltreeTextToLtreePictures LEmpty = LEmpty
ltreeTextToLtreePictures (LLeaf p) = LLeaf (textToPicture p)
ltreeTextToLtreePictures (LNode p l r) = LNode (textToPictureCircle p) (ltreeTextToLtreePictures l) (ltreeTextToLtreePictures r)


example7 :: IO()
example7 = drawingOf $ drawLTree $ ltreeTextToLtreePictures sampleLTree


--------------------------------------------------------------------------------

main :: IO ()
-- main = example1 -- visualisation of linked list
-- main = example2 -- visualisation of samplePictureTree
-- main = example3 -- visualisation of sampleTree with mapLeaves function
-- main = example4 -- visualisation of samplePictureTree to linked list
-- main = example5 -- visualisation of samplePictureList to tree
main = example6 -- visualisation of samplePictureList to balanced tree