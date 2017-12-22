> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE FlexibleInstances #-}
> module Chapter11 where

> import qualified TestLib

> t str = TestLib.testTrue ("Chapter11: " ++ str)

exercises: dog types

> data PugType = PugData
> data HuskyType a = HuskyData
> data DogueDeBordeaux doge =
>   DogueDeBordeaux doge

> data Doggies a
>   = Husky a
>   | Mastiff a
>   deriving (Eq, Show)

1. Doggies is a type constructor
2. Doggies is * -> *
3. Doggies String is *
4. Num a => Doggies a
5. Doggies Integer
6. Doggies String
7. it depends upon the context (used at type level or term level)
8. doge -> DogueDeBordeaux doge
9. DogueDeBordeaux String



Exercises: Vehicles

> data Price
>   = Price Integer
>   deriving (Eq, Show)

> data Manufacturer
>   = Mini
>   | Mazda
>   | Tata
>   deriving (Eq, Show)

> data Airline
>   = PapuAir
>   | CatapultsR'Us
>   | TakeYourChancesUnited
>   deriving (Eq, Show)

> data Vehicle
>   = Car Manufacturer Price
>   | Plane Airline
>   deriving (Eq, Show)

> myCar = Car Mini (Price 14000)
> urCar = Car Mazda (Price 20000)
> clownCar = Car Tata (Price 7000)
> doge = Plane PapuAir

1. Vehicle
2.

> isCar :: Vehicle -> Bool
> isCar (Car _ _) = True
> isCar _         = False

> isPlane :: Vehicle -> Bool
> isPlane = not . isCar

> areCars :: [Vehicle] -> [Bool]
> areCars = map isCar

3.

> getManu :: Vehicle -> Manufacturer
> getManu (Car manu _) = manu

4. An exception will be thrown
5.

> data Size
>   = Size Integer
>   deriving (Eq, Show)

> data Vehicle2
>   = Car2 Manufacturer Price
>   | Plane2 Airline Size
>   deriving (Eq, Show)

> myCar2 = Car2 Mini (Price 14000)
> urCar2 = Car2 Mazda (Price 20000)
> clownCar2 = Car2 Tata (Price 7000)
> doge2 = Plane2 PapuAir (Size 747)

> isCar2 :: Vehicle2 -> Bool
> isCar2 (Car2 _ _) = True
> isCar2 _         = False

> isPlane2 :: Vehicle2 -> Bool
> isPlane2 = not . isCar2

> areCars2 :: [Vehicle2] -> [Bool]
> areCars2 = map isCar2

> getManu2 :: Vehicle2 -> Manufacturer
> getManu2 (Car2 manu _) = manu


exercieses: cardinality


1. 1
2. 3
3. 2^16
4. Int has the same bounds as Int64 on my system; Integer has no bounds, it uses as much memory as needed to hold its value
5. did answer this already in 3? 256 is 2^8.

exercises: for example

> data Example = MakeExample deriving Show

1. MakeExample type is Example; Example is not in scope as data constructor
2. Example has show defined for it..

> data Example2 a = MakeExample2 a deriving Show

3. MakeExample2 is `a -> Example2 a`. it now accepts a parameter, and eventual type is determined by type of parameter



exercises: logic goats

1.

> class TooMany a where
>   tooMany :: a -> Bool

> newtype CCCombo =
>   CCCombo (Int, String) deriving (Eq, Show)

> instance TooMany CCCombo where
>   tooMany (CCCombo (i, s)) = (i + length s) > 10

> -- testing out the FlexibleInstances bit.
> instance TooMany (Int, String) where
>   tooMany (i, s) = (i + length s) > 10

> testLogicGoats1 = do
>   t "tooMany (Int, String) 1" $ True == (tooMany (10 :: Int, "Foo" :: String))
>   t "tooMany (Int, String) 2" $ tooMany (6 :: Int, "Foo" :: String) == False
>   t "tooMany CCCombo (Int, String) 1" $ True == tooMany (CCCombo (10 :: Int, "Foo"))
>   t "tooMany CCCombo (Int, String) 2" $ False == tooMany (CCCombo (5 :: Int, "Foo"))

2.

> instance TooMany (Int, Int) where
>   tooMany (i,j) = i + j > 42

> testLogicGoats2 = do
>   t "tooMany (Int, Int) 1" $ tooMany (10 :: Int, 44 :: Int ) == True
>   t "tooMany (Int, Int) 2" $ tooMany (6 :: Int, 5 :: Int) == False


3.

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x,y) = (x*y) == 42

(not in code because will not compile due to Num/Int overlapping instances)



Exercises: Pity the Bool

1. Cardinality of Bool is 2, so:
   2 + 2 = 4

2. 2 + 2^8


------------------------------------------------------------------------------------------


> data Fiction = Fiction deriving Show
> data Nonfiction = Nonfiction deriving Show
> data BookType = FictionBook Fiction
>               | NonfictionBook Nonfiction
>               deriving Show

> type AuthorName = String
> data Author = Author (AuthorName, BookType)


> data Author2
>   = Fiction2 AuthorName
>   | NonFiction2 AuthorName
>   deriving (Eq, Show)

Exercises: How does your garden grow?

> data FlowerType = Gardenia
>                 | Daisy
>                 | Rose
>                 | Lilac
>                 deriving Show

> type Gardener = String
> data Garden =
>   Garden Gardener FlowerType
>   deriving Show

normal form is:

> data FlowerType2 = Gardenia2 Gardener
>                 | Daisy2 Gardener
>                 | Rose2 Gardener
>                 | Lilac2 Gardener
>                 deriving Show


------------------------------------------------------------------------------------------

> data GuessWhat =
>   Chickenbutt
>   deriving (Eq, Show)

> data Id a =
>   MkId a
>   deriving (Eq, Show)

> data Product a b =
>   Product a b
>   deriving (Eq, Show)

> data Sum a b
>   = First a
>   | Second b
>   deriving (Eq, Show)

> data RecordProduct a b =
>   RecordProduct
>     { pfirst  :: a
>     , psecond :: b }
>   deriving (Eq, Show)

> newtype NumCow =
>   NumCow Int
>   deriving (Eq, Show)

> newtype NumPig =
>   NumPig Int
>   deriving (Eq, Show)

> data Farmhouse =
>   Farmhouse NumCow NumPig
>   deriving (Eq, Show)

> type Farmhouse' = Product NumCow NumPig

> newtype NumSheep =
>   NumSheep Int
>   deriving (Eq, Show)

> data BigFarmhouse =
>   BigFarmhouse NumCow NumPig NumSheep

> type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

> type Name = String
> type Age = Int
> type LovesMud = Bool

> type PoundsOfWool = Int

> data CowInfo =
>   CowInfo Name Age
>   deriving (Eq, Show)

> data PigInfo =
>   PigInfo Name Age LovesMud
>   deriving (Eq, Show)

> data SheepInfo =
>   SheepInfo Name Age PoundsOfWool
>   deriving (Eq, Show)

> data Animal
>   = Cow CowInfo
>   | Pig PigInfo
>   | Sheep SheepInfo
>   deriving (Eq, Show)

> type Animal' =
>   Sum CowInfo (Sum PigInfo SheepInfo)


------------------------------------------------------------------------------------------
Exercise: programmers

> data OperatingSystem
>   = GnuPlusLinux
>   | OpenBSDPlusNevermindJustBSDStill
>   | Mac
>   | Windows
>   deriving (Eq, Show)

> data ProgLang
>   = Haskell
>   | Agda
>   | Idris
>   | PureScript
>    deriving (Eq, Show)

> data Programmer =
>   Programmer
>     { os :: OperatingSystem
>     , lang :: ProgLang }
>     deriving (Eq, Show)


> allOperatingSystems :: [OperatingSystem]
> allOperatingSystems =
>   [ GnuPlusLinux
>   , OpenBSDPlusNevermindJustBSDStill
>   , Mac
>   , Windows
>   ]

> allLanguages :: [ProgLang]
> allLanguages =
>   [ Haskell
>   , Agda
>   , Idris
>   , PureScript]

> allProgrammers :: [Programmer]
> allProgrammers =  concat
>   (map (flip map allLanguages)
>        (map Programmer allOperatingSystems))

------------------------------------------------------------------------------------------
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
>   TestLib.testTrue "mapTree" $ mapTree (+1) testTree' == mapExpected

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
>   TestLib.testTrue "preorder" $ preorder testTree == [2, 1, 3]

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
>   TestLib.testTrue "inorder" $ inorder testTree == [1, 2, 3]

> testPostorder :: IO ()
> testPostorder =
>   TestLib.testTrue "postorder" $ postorder testTree == [1, 3, 2]

> testFoldTree :: IO ()
> testFoldTree =
>   TestLib.testTrue "foldTree" $ foldTree (+) 0 testTree == 6

Vigenere Cipeher
----------------

> runTests :: IO ()
> runTests = do
>   Chapter11.testLogicGoats1
>   Chapter11.testLogicGoats2
>   Chapter11.testMapTree
>   Chapter11.testPreorder
>   Chapter11.testInorder
>   Chapter11.testPostorder
>   Chapter11.testFoldTree
