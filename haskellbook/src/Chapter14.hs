module Chapter14 where

import Test.Hspec
import Test.QuickCheck

import System.Environment

main :: IO ()
main = withArgs [] $ hspec $ do
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
