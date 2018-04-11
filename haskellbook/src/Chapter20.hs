module Chapter20 where

import Test.Hspec
import Data.Monoid
import Data.Foldable

sum' :: (Foldable t, Num a)
     => t a
     -> a
sum' = foldr (+) 0

sum'' :: (Foldable t, Num a)
      => t a
      -> a
sum'' = getSum . foldMap Sum


product' :: (Foldable t, Num a)
         => t a
         -> a
product' = foldr (*) 1

product'' :: (Foldable t, Num a)
          => t a
          -> a
product'' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a)
      => a
      -> t a
      -> Bool
elem' a =
  foldr folder False
  where
    folder a' found =
      if found == False then
        a == a'
      else
        True

elem'' :: (Foldable t, Eq a)
       => a
       -> t a
       -> Bool
elem'' a = getAny . foldMap (Any . (==a))

main :: IO ()
main = hspec $ do
  it "sum" $ do
    sum' [1..4] `shouldBe` 10
    sum'' [1..5] `shouldBe` 15

  it "product" $ do
    product' [1..4] `shouldBe` 24
    product'' [1..5] `shouldBe` 120

  it "elem" $ do
    elem' 8 [1..10] `shouldBe` True
    elem' 11 [1..10] `shouldBe` False
    elem'' 8 [1..10] `shouldBe` True
    elem'' 11 [1..10] `shouldBe` False
