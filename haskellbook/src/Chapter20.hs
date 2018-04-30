module Chapter20 where

import Test.Hspec
import Data.Monoid
import Data.Semigroup
import Data.Foldable
import Data.Bool

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

minimum' :: (Foldable t, Ord a)
        => t a
        -> Maybe a
minimum' xs = helper xs'
  where
    xs' = toList' xs
    helper [] = Nothing
    helper xs = Just $ minimum xs

minimum'' :: (Foldable t, Ord a, Bounded a)
          => t a
          -> Maybe a
minimum'' = fmap getMin . foldMap (Just . Min)

maximum' :: (Foldable t, Ord a, Bounded a)
          => t a
          -> Maybe a
maximum' = fmap getMax . foldMap (Just . Max)

null' :: (Foldable t) => t a -> Bool
null' = foldr (const . const False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ len -> len + 1) 0

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldr mappend mempty

foldMap' :: (Foldable t, Monoid m)
         => (a -> m)
         -> t a
         -> m
foldMap' f = foldr folder mempty
  where folder a m = (f a) `mappend` m

data Constant a b
  = Constant b


-- foldMap or foldr
instance  Foldable (Constant a) where
  foldMap f (Constant b) = f b

data Two a b
  = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c
  = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b
  = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b `mappend` f b'

data Four' a b
  = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b `mappend` f b' `mappend` f b''

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool)
        -> t a
        -> f a
filterF f = foldMap mapper
  where
    mapper a = bool mempty (pure a) (f a)


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

  it "minimum'" $ do
    minimum' [5..10] `shouldBe` ((Just 5) :: Maybe Int)
    minimum'' [5..10] `shouldBe` ((Just 5) :: Maybe Int)
    minimum'' ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)

  it "maximum'" $ do
    maximum' [5..10] `shouldBe` ((Just 10) :: Maybe Int)
    maximum' ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)

  it "null" $ do
    null' [] `shouldBe` True
    null' [1] `shouldBe` False

  it "length'" $ do
    length' [] `shouldBe` 0
    length' [1..5] `shouldBe` 5

  it "toList'" $ do
    toList (Just 1) `shouldBe` ([1] :: [Int])
    toList Nothing `shouldBe` ([] :: [Int])

  it "fold'" $ do
    fold' ([1,2,3,4] :: [Sum Int]) `shouldBe` (Sum 10 :: Sum Int)
    fold' ([1,2,3,4] :: [Product Int]) `shouldBe` (Product 24 :: Product Int)

  it "foldMap'" $ do
    foldMap' (+1) ([1,2,3,4] :: [Sum Int]) `shouldBe` (Sum 14 :: Sum Int )

  it "Constant foldable" $ do
    foldMap id (Constant (Sum 1)) `shouldBe` (Sum 1)

  it "Two foldable" $ do
    foldMap id (Two 1 (Sum 1)) `shouldBe` (Sum 1)

  it "Three foldable" $ do
    foldMap id (Three 1 1 (Sum 1)) `shouldBe` (Sum 1)

  it "Three' foldable" $ do
    foldMap Sum (Three' 1 1 1) `shouldBe` (Sum 2)

  it "Four' foldable" $ do
    foldMap Sum (Four' 1 1 1 5) `shouldBe` (Sum 7)

  it "filterF" $ do
    filterF even ([1..5] :: [Int]) `shouldBe` [2,4]
