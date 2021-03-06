{-# LANGUAGE FlexibleInstances #-}

module Chapter11 where

import qualified TestLib
import qualified Chapter09 as C9
import Data.Char
import Data.List
import Data.Maybe

t str = TestLib.testTrue ("Chapter11: " ++ str)

-- exercises: dog types

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge =
  DogueDeBordeaux doge

data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

-- 1. Doggies is a type constructor
-- 2. Doggies is * -> *
-- 3. Doggies String is *
-- 4. Num a => Doggies a
-- 5. Doggies Integer
-- 6. Doggies String
-- 7. it depends upon the context (used at type level or term level)
-- 8. doge -> DogueDeBordeaux doge
-- 9. DogueDeBordeaux String



-- Exercises: Vehicles

data Price
  = Price Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1. Vehicle
-- 2.

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3.

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

-- 4. An exception will be thrown
-- 5.

data Size
  = Size Integer
  deriving (Eq, Show)

data Vehicle2
  = Car2 Manufacturer Price
  | Plane2 Airline Size
  deriving (Eq, Show)

myCar2 = Car2 Mini (Price 14000)
urCar2 = Car2 Mazda (Price 20000)
clownCar2 = Car2 Tata (Price 7000)
doge2 = Plane2 PapuAir (Size 747)

isCar2 :: Vehicle2 -> Bool
isCar2 (Car2 _ _) = True
isCar2 _         = False

isPlane2 :: Vehicle2 -> Bool
isPlane2 = not . isCar2

areCars2 :: [Vehicle2] -> [Bool]
areCars2 = map isCar2

getManu2 :: Vehicle2 -> Manufacturer
getManu2 (Car2 manu _) = manu


-- exercieses: cardinality


-- 1. 1
-- 2. 3
-- 3. 2^16
-- 4. Int has the same bounds as Int64 on my system; Integer has no bounds, it uses as much memory as needed to hold its value
-- 5. did answer this already in 3? 256 is 2^8.

-- exercises: for example

data Example = MakeExample deriving Show

-- 1. MakeExample type is Example; Example is not in scope as data constructor
-- 2. Example has show defined for it..

data Example2 a = MakeExample2 a deriving Show

-- 3. MakeExample2 is `a -> Example2 a`. it now accepts a parameter, and eventual type is determined by type of parameter



-- exercises: logic goats

-- 1.

class TooMany a where
  tooMany :: a -> Bool

newtype CCCombo =
  CCCombo (Int, String) deriving (Eq, Show)

instance TooMany CCCombo where
  tooMany (CCCombo (i, s)) = (i + length s) > 10

-- testing out the FlexibleInstances bit.
instance TooMany (Int, String) where
  tooMany (i, s) = (i + length s) > 10

testLogicGoats1 = do
  t "tooMany (Int, String) 1" $ True == (tooMany (10 :: Int, "Foo" :: String))
  t "tooMany (Int, String) 2" $ tooMany (6 :: Int, "Foo" :: String) == False
  t "tooMany CCCombo (Int, String) 1" $ True == tooMany (CCCombo (10 :: Int, "Foo"))
  t "tooMany CCCombo (Int, String) 2" $ False == tooMany (CCCombo (5 :: Int, "Foo"))

-- 2.

instance TooMany (Int, Int) where
  tooMany (i,j) = i + j > 42

testLogicGoats2 = do
  t "tooMany (Int, Int) 1" $ tooMany (10 :: Int, 44 :: Int ) == True
  t "tooMany (Int, Int) 2" $ tooMany (6 :: Int, 5 :: Int) == False


-- 3.

-- instance (Num a, TooMany a) => TooMany (a, a) where
--   tooMany (x,y) = (x*y) == 42

-- (commented out because will not compile due to Num/Int overlapping instances)



-- Exercises: Pity the Bool

-- 1. Cardinality of Bool is 2, so:
--   2 + 2 = 4

-- 2. 2 + 2^8


-- ------------------------------------------------------------------------------------------


data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType)


data Author2
  = Fiction2 AuthorName
  | NonFiction2 AuthorName
  deriving (Eq, Show)

-- Exercises: How does your garden grow?

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String
data Garden =
  Garden Gardener FlowerType
  deriving Show

-- normal form is:

data FlowerType2 = Gardenia2 Gardener
                | Daisy2 Gardener
                | Rose2 Gardener
                | Lilac2 Gardener
                deriving Show


-- ------------------------------------------------------------------------------------------

data GuessWhat =
  Chickenbutt
  deriving (Eq, Show)

data Id a =
  MkId a
  deriving (Eq, Show)

data Product a b =
  Product a b
  deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct
    { pfirst  :: a
    , psecond :: b }
  deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)


-- ------------------------------------------------------------------------------------------
-- Exercise: programmers

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
   deriving (Eq, Show)

data Programmer =
  Programmer
    { os :: OperatingSystem
    , lang :: ProgLang }
    deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [ Haskell
  , Agda
  , Idris
  , PureScript]

allProgrammers :: [Programmer]
allProgrammers =  concat
  (map (flip map allLanguages)
       (map Programmer allOperatingSystems))

testAllProgrammers = t "allProgrammers" $ 16 == length allProgrammers

-- ------------------------------------------------------------------------------------------
-- Convert Cardinality
-- -------------------

data Quantum
  = Yes
  | No
  | Both

convert1 :: Quantum -> Bool
convert1 Yes  = False
convert1 No   = False
convert1 Both = False

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = False
convert2 Both = False


convert3 :: Quantum -> Bool
convert3 Yes  = False
convert3 No   = True
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes  = True
convert4 No   = True
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes  = False
convert5 No   = False
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes  = True
convert6 No   = False
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = True
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes  = True
convert8 No   = True
convert8 Both = True

-- exercises quad p 672
-- 1. 4 + 4
-- 2. 4 * 4
-- 3. 4 ^ 4
-- 4. 4 * 4 * 4
-- 5. 2 ^ (2 * 2)
-- 6. 4 ^ (4 * 2) = (2^2)^(4*2) = 2^(2*4*2) = 2^16 = 65536



-- Binary Tree
-- -----------

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a
        -> BinaryTree a
        -> BinaryTree a

insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
testMapTree =
  TestLib.testTrue "mapTree" $ mapTree (+1) testTree' == mapExpected

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  TestLib.testTrue "preorder" $ preorder testTree == [2, 1, 3]

foldTree :: (a -> b -> b) ->
            b ->
            BinaryTree a ->
            b
foldTree f b tree =
  case tree of
    Leaf ->
      b
    Node left a right ->
      let
        fa = f a b
        fLeft = foldTree f fa left
        fRight = foldTree f fLeft right
      in
        fRight

testInorder :: IO ()
testInorder =
  TestLib.testTrue "inorder" $ inorder testTree == [1, 2, 3]

testPostorder :: IO ()
testPostorder =
  TestLib.testTrue "postorder" $ postorder testTree == [1, 3, 2]

testFoldTree :: IO ()
testFoldTree =
  TestLib.testTrue "foldTree" $ foldTree (+) 0 testTree == 6


-- chapter exercises:

-- 1. a
-- 2. c
-- 3. b
-- 4. c

-- Vigenere Cipeher
-- ----------------

calculateOffset :: Char -> Int
calculateOffset c =
  if isAlpha c then
    if isLower c then
      ord c - ord 'a'
    else
      ord c - ord 'A'
  else
    0

calculateOffsets :: String -> [Int]
calculateOffsets "" = []
calculateOffsets (c:cs) =
  (calculateOffset c) : calculateOffsets cs

testCalculateOffsets = do
  t "calculateOffsets" $ calculateOffsets "AbBa" == [0,1,1,0]

vigenere :: String -> String -> String
vigenere = (vigenere' id)

unVigenere :: String -> String -> String
unVigenere = (vigenere' (*(-1)))

vigenere' :: (Int -> Int) -> String -> String -> String
vigenere' _ "" message = message
vigenere' modifier cipherText message =
  let
    offsets :: [Int]
    offsets = calculateOffsets cipherText ++ offsets
  in
    map (uncurry C9.shiftChar) $ zip (map modifier offsets) message

testVigenere = do
  t "vigenere" $ vigenere "ALLY" "meetatdawn" == "mppraeoywy"
  t "vigenere2" $ "AZFOOza" == (unVigenere "ALLY" $ vigenere "ALLY" "AZFOOza")

-- as patterns exercises

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf as@(a:aa) bs@(b:bb)
    | a == b = isSubseqOf aa bb
    | otherwise = isSubseqOf as bb

testIsSubseqOf = do
  t "subseq"  $ isSubseqOf ("blah" :: String) "wboloath"
  t "subseq2" $ not $ isSubseqOf ("blah" :: String) "halbwoot"
  t "subseq3" $ not $ isSubseqOf ("blah" :: String) "wootbla"


capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capPair . C9.myWords
  where capPair xs@(x:xx) = (xs, toUpper x : xx)

testCapitalizeWords =
  t "tcw" $ capitalizeWords "hello world" == [("hello", "Hello"), ("world", "World")]


capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

testCapitalizeWord = do
  t "capitalizeword" $ capitalizeWord "Chortle" == "Chortle"
  t "capword" $ capitalizeWord "chortle" == "Chortle"

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph all@(x:xs)
  | x == ' ' = x : capitalizeParagraph xs
  | x == '.' = x : capitalizeParagraph xs
  | otherwise = capitalizeWord thisSentence ++ capitalizeParagraph remaining
     where thisSentence = (takeWhile (/= '.') all)
           remaining    = (dropWhile (/= '.') all)

testCapitalizeParagraph = t "cappara" $ "Blah. Woot ha." == capitalizeParagraph "blah. woot ha."


-- Phone exercise
-- 1.


type ButtonConfig = (Digit, String)

type DaPhone = [ButtonConfig]

daPhone :: DaPhone
daPhone =
  [ ('1', "")
  , ('2', "abc")
  , ('3', "def")
  , ('4', "ghi")
  , ('5', "jkl")
  , ('6', "mno")
  , ('7', "pqrs")
  , ('8', "tuv")
  , ('9', "wxyz")
  , ('*', "")
  , ('0', " ")
  , ('#', ".,")
  ]

-- 2.

type ButtonPresses = (Digit, Presses)

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

findButton :: Char
           -> Maybe ButtonConfig
findButton char =
  find checkElem daPhone
  where
    checkElem (digit,chars) =
      elem char (digit : chars)

charsAvailable :: ButtonConfig -> String
charsAvailable (digit, chars) =
  chars ++ [digit]

reverseTaps :: Char
            -> [ButtonPresses]
reverseTaps char =
  fromMaybe [] $ ((findButton lChar) >>= necessaryPresses)
    where
      lChar = toLower char

      maybeAddUpper :: [ButtonPresses] -> [ButtonPresses]
      maybeAddUpper buttonPresses
        | isUpper char = ('*', 1) : buttonPresses
        | otherwise    = buttonPresses

      necessaryPresses button = maybeAddUpper <$> (: []) <$> toPresses button lChar

toPresses :: ButtonConfig -> Char -> Maybe ButtonPresses
toPresses config@(digit, _) char =
  Just (digit, toPressesCount config char)

toPressesCount :: ButtonConfig -> Char -> Presses
toPressesCount config@(digit, _) char =
  fromMaybe (-100) $ (+1) <$> (elemIndex (toLower char) (charsAvailable config))

cellPhonesDead :: String
               -> [ButtonPresses]
cellPhonesDead = (>>= reverseTaps)

testCellPhonesDead =
  t "cellPhone" $ (take 7 $ cellPhonesDead (head convo)) == [('*',1),('9',1),('2',1),('6',2),('6',2),('2',1),('0',1)]

-- 3.

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd ) 0

testFingerTaps =
  t "fintaps" $ fingerTaps (take 7 $ cellPhonesDead (head convo)) == 9

-- 4.

mostPopularLetter :: String -> Char
mostPopularLetter = mostFrequent

mostFrequent :: Eq a => [a] -> a
mostFrequent xs =
  fst $ C9.myMaximumBy compareCounts freq
  where
    freq = frequency [] xs
    compareCounts a b = compare (snd a) (snd b)

frequency :: Eq a => [(a, Int)] -> [a] -> [(a,Int)]
frequency counts [] = counts
frequency counts (a:as) = frequency (incFreq a counts) as
  where
    incFreq char [] = [(char, 1)]
    incFreq char ((c, num) : frequencies)
      | char == c = (c, num + 1) : frequencies
      | otherwise = (c, num) : incFreq char frequencies

testMostPopularLetter =
  t "mpl" $ fmap mostPopularLetter convo == " aa a  a "

calcCost :: Char -> Presses
calcCost char = fromMaybe (-1000) $ flip toPressesCount char <$> findButton char

testCosts =
  t "costs" $ fmap (calcCost . mostPopularLetter) convo == [1,1,1,1,1,1,1,1,1]

-- 5.

coolestLtr :: [String] -> Char
coolestLtr convo = mostPopularLetter $ concat convo

testCoolestLtr =
  t "cltr" $ coolestLtr convo == ' '

coolestWord :: [String] -> String
coolestWord convo =
  mostFrequent $ C9.myWords $ concat $ intersperse " " convo

testCoolestWord =
  t "cword" $ coolestWord convo == "Lol"


-- hutton's razor

-- 1.

data Expr
  = Lit Integer
  | Add Expr Expr

hEval :: Expr -> Integer
hEval (Lit i) = i
hEval (Add e1 e2) = hEval e1 + hEval e2

testEval =
  t "huttonEval" $ hEval (Add (Lit 1) (Lit 9001))  == 9002

-- 2.

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2

testPrint = do
  t "p1" $ printExpr (Add (Lit 1) (Lit 9001)) == "1 + 9001"
  let a1 = Add (Lit 9001) (Lit 1)
  let a2 = Add a1 (Lit 20001)
  let a3 = Add (Lit 1) a2
  t "pa2" $ printExpr a3 == "1 + 9001 + 1 + 20001"

runTests :: IO ()
runTests = do
  testLogicGoats1
  testLogicGoats2
  testAllProgrammers
  testMapTree
  testPreorder
  testInorder
  testPostorder
  testFoldTree
  testCalculateOffsets
  testVigenere
  testIsSubseqOf
  testCapitalizeWords
  testCapitalizeWord
  testCapitalizeParagraph
  testCellPhonesDead
  testFingerTaps
  testMostPopularLetter
  testCosts
  testCoolestLtr
  testCoolestWord
  testEval
  testPrint
