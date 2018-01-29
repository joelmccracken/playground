module Chapter14 where

import Test.Hspec

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

main :: IO ()
main = hspec $ do
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
