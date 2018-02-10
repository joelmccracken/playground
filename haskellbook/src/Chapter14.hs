{-# LANGUAGE DeriveGeneric #-}

module Chapter14 where

import qualified Morse

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Gen (oneof)

import GHC.Generics

import qualified Chapter08 as C08
import qualified Chapter09 as C09
import qualified Chapter11 as C11


import Data.Function ((&))

import Data.List (sort)
import Hangman

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
        it "exponentiation associativity" $ do
          expectFailure $ property (associative (^) :: Int -> Int -> Int -> Bool)
        it "exponentiation commutivity" $ do
          expectFailure $ property (commutative (^) :: Int -> Int -> Bool)

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
              ((f' . g') x) == (\x2 -> f' (g' x2)) x
              where
                f' = apply f
                g' = apply g
          property test

        it "is foldr (:) == (++)" $ do
          expectFailure $ property $ \xs ys ->
            (foldr (:)) (xs :: [Int]) (ys :: [Int]) == (++) xs ys
          -- not the same, might be the same if flipped as (flip (foldr (:)))

        -- nope, wont be same if n > length of xs or n < 0
        it "is foldr (++) [] == concat" $ do
          let
            f :: Int -> [Int] -> Bool
            f n xs = length (take n xs) == n
          expectFailure $ property f

        it "show read round-trip" $ do
          let
            roundTrip :: (Read a, Show a, Eq a) => a -> Bool
            roundTrip x =
              read (show x) == x
          (property (roundTrip :: Int -> Bool)
           .&&.
           property (roundTrip :: String -> Bool) )

        it "why property fails" $ do
          let square x = x * x
              squareId = square . sqrt
              prop x = x == squareId x
          expectFailure $ property (prop :: Float -> Bool)
          -- fails because of inexact precision with small floating numbers

        describe "idempotence" $ do
          let twice f = f . f
              fourTimes = twice . twice
          it "cap word" $ do
            let f x =
                  (C11.capitalizeWord x
                    == twice C11.capitalizeWord x)
                  &&
                  (C11.capitalizeWord x
                   == fourTimes C11.capitalizeWord x)
            property f
          it "sort" $ do
            let f :: [Int] -> Bool
                f x =
                  (sort x == twice sort x)
                  &&
                  (sort x == fourTimes sort x)
            property f

        describe "testing hangman" $ do
          it "fills in a character" $ do
            let start = Puzzle "a" [Nothing] "ei"
            let end =   Puzzle "a" [Just 'a'] "aei"
            fillInCharacter start 'a' `shouldBe` end

          it "fills in duplicates" $ do
            let start = Puzzle "abba" [Nothing, Just 'b', Just 'b', Nothing] ""
            let end =   Puzzle "abba" [Just 'a', Just 'b', Just 'b', Just 'a'] "a"
            fillInCharacter start 'a' `shouldBe` end

          it "only adds to guessed list when guessed is not in puzzle" $ do
            let start = Puzzle "xy" [Nothing, Nothing] ""
            let end =   Puzzle "xy" [Nothing, Nothing] "a"
            fillInCharacter start 'a' `shouldBe` end

          it "always adds characters to the end" $ do
            let guessedIsAlwaysAtEnd :: Char -> Bool
                guessedIsAlwaysAtEnd guess =
                  let
                    result = fillInCharacter (Puzzle "" [] "") guess
                  in
                    (\(Puzzle _ _ (x:xs)) -> x == guess) result
            property guessedIsAlwaysAtEnd

          it "finds if guess is in word" $ do
            let start = Puzzle "abba" [Nothing, Nothing] ""
            let wasInWord =
                  case handleGuess' start 'a' of
                    WasInWord _ -> True
                    _ -> False
            wasInWord `shouldBe` True

          it "finds if guess is not in word" $ do
            let start = Puzzle "abba" [Nothing, Nothing] ""
            let wasInWord =
                  case handleGuess' start 'x' of
                    WasNotInWord _ -> True
                    _ -> False
            wasInWord `shouldBe` True

          it "finds if guess was already guessed" $ do
            let start = Puzzle "abba" [Nothing, Nothing] "x"
            let wasInWord =
                  case handleGuess' start 'x' of
                    AlreadyGuessed _ -> True
                    _ -> False
            wasInWord `shouldBe` True

        describe "testing ciphers" $ do
          it "vigenere" $ do
            let prop :: String -> String -> Bool
                prop cipherText message =
                  (C11.unVigenere cipherText (C11.vigenere cipherText message)) == message
            property prop

          it "caesar" $ do
            let prop :: String -> String -> Bool
                prop cipherText message =
                  (C09.caesar (-10) (C09.caesar 10 message)) == message
            property prop



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


-- gen exercises

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

foolGen = elements [Fulse, Frue]
weightedFoolGen =
  frequency [(2, return Fulse),
             (1, return Frue)]
