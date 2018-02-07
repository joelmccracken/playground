{-# LANGUAGE DeriveGeneric #-}

module Chapter14 where

import qualified Morse

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Gen (oneof)

import GHC.Generics
import Test.QuickCheck

import qualified Chapter08 as C08

import Data.List (sort)

main :: IO ()
main =  do
  hspec $ do
    describe "chapter exercises" $ do
      describe "using quickcheck" $ do
        it "halfIdentity" $ do
          let halfIdentity = (*2) . half
          property $ \x -> x == halfIdentity (x :: Float)
        it "listOrdered" $ do
          property $ \x -> listOrdered (sort (x :: [Int]))
        it "addition associativity" $ do
          property (associative (+) :: Int -> Int -> Int -> Bool)
        it "addition commutivity" $ do
          property (commutative (+) :: Int -> Int -> Bool)
        it "multiplication associativity" $ do
          property (associative (*) :: Int -> Int -> Int -> Bool)
        it "multiplication commutivity" $ do
          property (commutative (*) :: Int -> Int -> Bool)
        it "quot rem properties" $ do
          let quotRemProp :: Int -> Int -> Bool
              quotRemProp x y =
                (quot x y') * y' + (rem x y') == x
                where y' = notZero y
          property quotRemProp
        it "div mod properties" $ do
          let divModProp :: Int -> Int -> Bool
              divModProp x y =
                (div x y') * y' + (mod x y') == x
                where y' = notZero y
          property divModProp

        -- neither associative nor commutative
        -- it "multiplication associativity" $ do
        --   property (associative (^) :: Int -> Int -> Int -> Bool)
        -- it "multiplication commutivity" $ do
        --   property (commutative (^) :: Int -> Int -> Bool)

        it "double reverse is id" $ do
          let doubleReverseIsId :: [Int] -> Bool
              doubleReverseIsId xs =
                (reverse . reverse) xs == id xs
          property doubleReverseIsId

        it "property for the definition of ($)" $ do
          let
            test :: Fun Int Int -> Int -> Bool
            test fn x =
              ((apply fn) x) == ((apply fn) $ x)
          property test

        it "property for the definition of (.)" $ do
          let
            test :: Fun Int Int -> Fun Int Int -> Int -> Bool
            test f g x =
              ((f' . g') x) == (\x2 -> (f' (g' x2)) x
              where
                f' = apply f
                g' = apply g
          property test

    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ do
        (1 + 1) > 1 `shouldBe` True
      it "2 + 2 is equal to 4" $ do
        2 + 2 `shouldBe` 4
    describe "Division" $ do
      it "15 dividedby 3 is 5" $ do
        dividedBy 15 3 `shouldBe` (5, 0)
      it "22 divided by 5 is\
         \ 4 remainder 2" $ do
        dividedBy 22 5 `shouldBe` (4, 2)
    describe "recurMult" $ do
      it "does integral multiplication" $ do
        recurMult 15 3 `shouldBe` 45
        recurMult 15 (-3) `shouldBe` (-45)
        recurMult (-15) (-3) `shouldBe` (45)

    it "x + 1 is always\
       \ greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

    describe "chapter exercises" $ do
      describe "digitToWord" $ do
        it "returns zero for 0" $ do
          C08.digitToWord 0 `shouldBe` "zero"
        it "returns one for 1" $ do
          C08.digitToWord 1 `shouldBe` "one"
      describe "digits" $ do
        it "returns [1] for 1" $ do
          C08.digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $ do
          C08.digits 100 `shouldBe` [1, 0, 0]
      describe "wordNumber" $ do
        it "one-zero-zero given 100" $ do
          C08.wordNumber 100 `shouldBe` "one-zero-zero"
        it "nine-zero-zero-one for 9001" $ do
          C08.wordNumber 9001`shouldBe` "nine-zero-zero-one"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom =
  go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise =
          go (n - d) d (count + 1)

recurMult :: (Eq a, Num a) => a -> a -> a
recurMult _ 0 = 0
recurMult 0 _ = 0
recurMult x y =
  recurMult' y'
  where
  signModifierY = signum y
  x' = x * signModifierY
  y' = abs y
  recurMult' 0 = 0
  recurMult' y2 =
    x' + recurMult' (y2 - 1)


genBool :: Gen Bool
genBool = choose (False, True)
genBool' :: Gen Bool
genBool' = elements [False, True]
genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]
genChar :: Gen Char
genChar = elements ['a'..'z']


genEither :: (Arbitrary a, Arbitrary b)
          => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]


genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return $ Just a)
            ]


-- arbitrary instances


data Trivial =
  Trivial
  deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen =
  return Trivial


instance Arbitrary Trivial where
  arbitrary = trivialGen



data Identity a =
  Identity a
  deriving (Eq, Show)


identityGen :: Arbitrary a =>
               Gen (Identity a)

identityGen = do
  a <- arbitrary
  return (Identity a)


instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = identityGen


identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

data Pair a b =
  Pair a b
  deriving (Eq, Show)

pairGen :: (Arbitrary a,
            Arbitrary b) =>
           Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a,
         Arbitrary b) =>
         Arbitrary (Pair a b) where
  arbitrary = pairGen


pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen


data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)


sumGenEqual :: (Arbitrary a,
                Arbitrary b) =>
               Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a,
         return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual


sumGenFirstPls:: (Arbitrary a,
                  Arbitrary b) =>
                 Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a),
             (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

data Bool'
  = True'
  | False'
  deriving (Generic)


instance CoArbitrary Bool'


trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary

half x = x / 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)


associative :: Eq a
            => (a -> a -> a)
            -> a -> a -> a
            -> Bool
associative f x y z =
  (x `f` y) `f` z == (x `f` y) `f` z


commutative :: Eq a
            => (a -> a -> a)
            -> a -> a
            -> Bool
commutative f x y =
  x `f` y ==  y `f` x

notZero 0 = 1
notZero i = i
