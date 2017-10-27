> module Chapter11 where

Convert Cardinality
-------------------

> data Quantum
>   = Yes
>   | No
>   | Both

> convert1 :: Quantum -> Bool
> convert1 Yes  = False
> convert1 No   = False
> convert1 Both = False

> convert2 :: Quantum -> Bool
> convert2 Yes  = True
> convert2 No   = False
> convert2 Both = False


> convert3 :: Quantum -> Bool
> convert3 Yes  = False
> convert3 No   = True
> convert3 Both = False

> convert4 :: Quantum -> Bool
> convert4 Yes  = True
> convert4 No   = True
> convert4 Both = False

> convert5 :: Quantum -> Bool
> convert5 Yes  = False
> convert5 No   = False
> convert5 Both = True

> convert6 :: Quantum -> Bool
> convert6 Yes  = True
> convert6 No   = False
> convert6 Both = True

> convert7 :: Quantum -> Bool
> convert7 Yes  = False
> convert7 No   = True
> convert7 Both = True

> convert8 :: Quantum -> Bool
> convert8 Yes  = True
> convert8 No   = True
> convert8 Both = True

exercises quad p 672
1. 4 + 4
2. 4 * 4
3. 4 ^ 4
4. 4 * 4 * 4
5. 2 ^ (2 * 2)
6. 4 ^ (4 * 2) = (2^2)^(4*2) = 2^(2*4*2) = 2^16 = 65536



Binary Tree
-----------

> data BinaryTree a
>   = Leaf
>   | Node (BinaryTree a) a (BinaryTree a)
>   deriving (Eq, Ord, Show)

> insert' :: Ord a => a
>         -> BinaryTree a
>         -> BinaryTree a

> insert' b Leaf = Node Leaf b Leaf
> insert' b (Node left a right)
>   | b == a = Node left a right
>   | b < a  = Node (insert' b left) a right
>   | b > a  = Node left a (insert' b right)

> mapTree :: (a -> b)
>         -> BinaryTree a
>         -> BinaryTree b
> mapTree _ Leaf = Leaf
> mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

> testTree' :: BinaryTree Integer
> testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

> mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

> -- acceptance test for mapTree
> testMapTree =
>   if mapTree (+1) testTree' == mapExpected
>   then putStrLn "mapTree: yup okay!"
>   else error "mapTree: test failed!"

> preorder :: BinaryTree a -> [a]
> preorder Leaf = []
> preorder (Node left a right) = a : (preorder left) ++ (preorder right)

> inorder :: BinaryTree a -> [a]
> inorder Leaf = []
> inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

> postorder :: BinaryTree a -> [a]
> postorder Leaf = []
> postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

> testTree :: BinaryTree Integer
> testTree = Node
>     (Node Leaf 1 Leaf)
>     2
>     (Node Leaf 3 Leaf)

> testPreorder :: IO ()
> testPreorder =
>   if preorder testTree == [2, 1, 3] then
>     putStrLn "Preorder fine!"
>   else
>     putStrLn "Bad news bears."

> foldTree :: (a -> b -> b) ->
>             b ->
>             BinaryTree a ->
>             b
> foldTree f b tree =
>   case tree of
>     Leaf ->
>       b
>     Node left a right ->
>       let
>         fa = f a b
>         fLeft = foldTree f fa left
>         fRight = foldTree f fLeft right
>       in
>         fRight

> testInorder :: IO ()
> testInorder =
>   if inorder testTree == [1, 2, 3] then
>     putStrLn "Inorder fine!"
>   else
>     putStrLn "Bad news bears."

> testPostorder :: IO ()
> testPostorder =
>   if postorder testTree == [1, 3, 2] then
>     putStrLn "Postorder fine!"
>   else
>     putStrLn "Bad news bears."

> testFoldTree :: IO ()
> testFoldTree =
>   if foldTree (+) 0 testTree == 6 then
>     putStrLn "foldTree fine!"
>   else
>     putStrLn "Bad news bears."

Vigenere Cipeher
----------------

> runTests :: IO ()
> runTests = do
>   Chapter11.testMapTree
>   Chapter11.testPreorder
>   Chapter11.testInorder
>   Chapter11.testPostorder
>   Chapter11.testFoldTree
