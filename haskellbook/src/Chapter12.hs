module Chapter12 where

import qualified TestLib
import qualified Chapter09 as C9
import Data.Char
import Data.List
import Data.Bool (bool)
import Data.Maybe (Maybe, fromMaybe)
import Data.Function ((&))

import Data.Either (Either, either)
t str = TestLib.testTrue ("Chapter12: " ++ str)

-- determine the kinds
-- 1. a is *
-- 2. a is *, f is * -> *


-- String Processing

-- 1.
notThe :: String -> Maybe String
notThe str =
  case (str == "the") of
    True  -> Nothing
    False -> Just str

replaceThe :: String -> String
replaceThe str = intercalate " " $ map (fromMaybe "a") $ (map notThe $ C9.myWords str)

testThe = do
  t "nt1" $ notThe "the" == Nothing
  t "nt2" $ notThe "blahtheblah" == Just "blahtheblah"
  t "nt3" $ notThe "blahtheblah" == Just "blahtheblah"
  t "nt4" $ notThe "woot" == Just "woot"
  t "nt5" $ replaceThe "the cow loves us" == "a cow loves us"

-- 2.

isVowel :: Char -> Bool
isVowel = flip C9.myElem $ "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel input =
  input & C9.myWords & countHelper 0
  where
    countHelper n (a : bb@(b : bs) : xs) =
      if a == "the" && isVowel b then
        countHelper (n+1) xs
      else
        countHelper n (bb:xs)
    countHelper n _ = n

testCountTheBeforeVowel = do
 t "cbv" $ countTheBeforeVowel "the cow" == 0
 t "cbv2" $ countTheBeforeVowel "the evil cow" == 1

-- 3.

countVowels :: String -> Integer
countVowels str =
  str &
  map (bool 0 1 . isVowel) &
  sum

testCountVowels = do
  t "cv" $ countVowels "the cow" == 2
  t "cv2" $ countVowels "Mikolajczak" == 4


-- validate the word

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word =
  case compare numVowels numConsonants of
    GT -> Nothing
    _  -> Just $ Word' word
  where
    numVowels = (countVowels word)
    numConsonants = (word & length & toInteger) - numVowels

testMkWord = do
  t "mkw" $ mkWord "the" == Just (Word' "the")
  t "mkw2" $ mkWord "a" == Nothing

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

testNatToInteger = do
 t "ntoi" $ natToInteger Zero == 0
 t "ntoi" $ natToInteger (Succ Zero) == 1
 t "ntoi" $ natToInteger (Succ (Succ Zero)) == 2

natToInteger :: Nat -> Integer
natToInteger nat =
  case nat of
    Succ a -> 1 + natToInteger a
    Zero -> 0

testIntegerToNat = do
  t "iton" $ integerToNat 0 == Just Zero
  t "iton" $ integerToNat 1 == Just (Succ Zero)
  t "iton" $ integerToNat 2 == Just (Succ (Succ Zero))
  t "iton" $ integerToNat (-1) == Nothing

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just $ iToNHelper i
  where
    iToNHelper 0 = Zero
    iToNHelper i = Succ $ iToNHelper (i - 1)

-- Small library for Maybe

-- 1.

testIsJust = do
  t "isJust" $ isJust (Just 1) ==  True
  t "isJust" $ isJust Nothing == False

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing  = False

testIsNothing = do
  t "isNothing" $ isNothing (Just 1) == False
  t "isNothing" $ isNothing Nothing ==  True

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing  = True

-- 2.

testMayybee = do
  t "mayybee" $ mayybee 0 (+1) Nothing == 0
  t "mayybee" $ mayybee 0 (+1) (Just 1) == 2

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b aToB maybeA =
  case maybeA of
    Just a  -> aToB a
    Nothing -> b

-- 3.

testFromMaybe = do
  t "fromMaybe" $ fromMaybe2 0 Nothing == 0
  t "fromMaybe" $ fromMaybe2 0 (Just 1) == 1

fromMaybe2 :: a -> Maybe a -> a
fromMaybe2 a maybeA =
  mayybee a id maybeA

-- 4.

testListToMaybe = do
  t "listToMaybe" $ listToMaybe [1, 2, 3] == Just 1
  t "listToMaybe" $ listToMaybe ([] :: [Int]) == Nothing

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:as) = Just a

testMaybeToList = do
  t "maybeToList" $ maybeToList (Just 1) == [1]
  t "maybeToList" $ maybeToList Nothing == ([] :: [Int])

maybeToList :: Maybe a -> [a]
maybeToList ma = mayybee [] (:[]) ma


-- 5.

testCatMaybes = do
  t "catMaybes" $ catMaybes [Just 1, Nothing, Just 2] == [1, 2]
  t "catMaybes" $ catMaybes (take 3 $ repeat Nothing) == ([] :: [Int])

catMaybes :: [Maybe a] -> [a]
catMaybes maybes =
  foldr folder [] maybes
  where
    folder maybeA as = mayybee as (:as) maybeA

-- 6.

testFlipMaybe = do
  t "flip" $ flipMaybe [Just 1, Just 2, Just 3] == Just [1, 2, 3]
  t "flip" $ flipMaybe [Just 1, Nothing, Just 3] == Nothing

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe maybes =
  foldr folder (Just []) maybes
  where
    folder maybeA maybeAs =
      case maybeAs of
        Just as ->
          case maybeA of
            Just a -> Just (a:as)
            Nothing -> Nothing
        Nothing -> Nothing

-- Small library for Either


-- 1.

lefts' :: [Either a b] -> [a]
lefts' eithers =
  foldr folder [] eithers
  where
    folder eitherAOrB as =
      either (:as) (const as) eitherAOrB

testLefts = do
  t "lefts" $ lefts' [Left 1, Right 10] == ([1] :: [Integer])
  t "lefts" $ lefts' [Right 2] == ([] :: [Integer])

-- 2.

rights' eithers =
  foldr folder [] eithers
  where
    folder eitherAOrB as =
      either (const as) (:as) eitherAOrB

testRights = do
  t "rights" $ rights' [Left 1, Right 10] == ([10] :: [Integer])
  t "rights" $ rights' [Left 2] == ([] :: [Integer])

-- 3.

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts' eithers, rights' eithers)

testPartitionEithers = do
  t "partitionEithers" $ partitionEithers' [Left 'a', Right False] == (['a'], [False])
  t "partitionEithers" $ partitionEithers' [Left 'a'] == (['a'], [] :: [Char])

-- 4.

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' bToC eAorB =
  case eAorB of
    Left a -> Nothing
    Right b -> Just $ bToC b

testEitherMaybe = do
  t "eitherMaybe" $ eitherMaybe' id (Left 'a' :: Either Char Char) == Nothing
  t "eitherMaybe" $ eitherMaybe' id (Right 'a' :: Either Char Char) == Just 'a'

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' aToC bToC eAB =
  case eAB of
    Left a -> aToC a
    Right b -> bToC b

testEither' = do
  t "either" $ either' (const False) (const True) (Left 1) == False
  t "either" $ either' (const False) (const True) (Right 1) == True

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe''  bToC eAB =
  either' (const Nothing) (Just . bToC) eAB

testEitherMaybe'' = do
  t "eitherMaybe''" $ eitherMaybe'' id (Left 'a' :: Either Char Char) == Nothing
  t "eitherMaybe''" $ eitherMaybe'' id (Right 'a' :: Either Char Char) == Just 'a'




-- anamorphisms

-- 1.
myIterate :: (a -> a)
          -> a
          -> [a]

myIterate succ this =
  this : myIterate succ (succ this)

testMyIterate = do
  t "myIterate" $ (myIterate (+1) 0 & take 10) == [0,1,2,3,4,5,6,7,8,9]


-- 2.

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr iter step =
  case iter step of
    Nothing -> []
    Just (a,b) -> a : myUnfoldr iter b

testMyUnfoldr = do
  t "myUnfoldr" $ myUnfoldr foo (ord 'a') == [0..24]
  where
    foo oChar =
      if oChar < (ord 'z') then
        Just (oChar - (ord 'a'), oChar+1)
      else
        Nothing

-- 3.

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x =
  myUnfoldr (\y -> Just (y, f y)) x

testBetterIterate = do
  t "betterIterate" $ (betterIterate (+1) 0 & take 10) == [0,1,2,3,4,5,6,7,8,9]


-- "finally something other than list"


data BinaryTree a
  = Leaf
  | Node
    (BinaryTree a)
    a
    (BinaryTree a)
  deriving (Eq, Ord, Show)


-- 1 .

unfold :: (a -> Maybe (a,b,a))
       -> a
       -> BinaryTree b
unfold f x =
  case f x of
    Nothing -> Leaf
    Just (a, b, c) -> Node (unfold f a) b (unfold f c)

testUnfold = do
  t "unfold" $ (unfold foo 'a') == (Node Leaf 'a' Leaf)
  where
    foo char =
      bool Nothing (Just (succ char, char, succ char)) (char < 'b')

-- 2.

treeBuild :: Integer -> BinaryTree Integer
treeBuild num =
  unfold build' 0
  where
    build' n =
      bool (Just ((n+1), n, (n+1))) Nothing  (n >= num)


testTreeBuild =
  t "treeBuild" $ treeBuild 3 ==
    (Node
     (Node
      (Node Leaf 2 Leaf)
      1
      (Node Leaf 2 Leaf))
     0
     (Node
      (Node Leaf 2 Leaf)
      1
      (Node Leaf 2 Leaf)))

runTests :: IO ()
runTests = do
  testThe
  testCountTheBeforeVowel
  testCountVowels
  testMkWord
  testNatToInteger
  testIntegerToNat
  testIsJust
  testIsNothing
  testMayybee
  testFromMaybe
  testListToMaybe
  testMaybeToList
  testCatMaybes
  testFlipMaybe
  testLefts
  testRights
  testPartitionEithers
  testEitherMaybe
  testEither'
  testEitherMaybe''
  testMyIterate
  testMyUnfoldr
  testBetterIterate
  testUnfold
  testTreeBuild
