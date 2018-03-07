{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chapter16 where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Function


{-
exercises: be kind

1. *
2. b: (* -> *), T: (* -> *)
3. * -> * -> *
-}

replaceWithP = const 'p'
rwp = replaceWithP

n = Nothing
w = Just "woohoo"
ave = Just "ave"
lms = [ave, n, w]

ha = Just ["Ha", "Ha"]
lmls = [ha, Nothing, Just []]

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
functorCompose f g x =
  fmap (f . g) x  == fmap f (fmap g x)

functorCompose' :: (Functor f, Eq (f c)) => Fun b c -> Fun a b -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x =
  fmap (f . g) x == fmap f (fmap g x)


newtype Identity a = Identity a
  deriving (Show, Eq, Arbitrary)

instance Functor Identity where
  fmap f (Identity a) =  Identity (f a)

data Pair a = Pair a a
  deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

main :: IO ()
main = hspec $ do
  describe "chapter content" $ do
    it "example of stacking fmaps" $ do
      let tripFmap = (fmap . fmap . fmap) :: (Char -> Char) -> [Maybe String] -> [Maybe String]
      tripFmap rwp lms `shouldBe` [Just "ppp", Nothing, Just "pppppp"]

  {-
  working out types for nested fmaps
  (fmap . fmap)
  :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)

  fmap :: (Functor f) => (a -> b) -> f a -> f b
  fmap' :: (Functor f') => (a' -> b') -> f' a' -> f' b'
  (.) :: (b -> c) -> (a -> b) -> a -> c

  fmap . fmap ::
  (Functor f, Functor f')
  => ((a -> b) (-> f' (f a) -> f' (f b))
  -> ((a -> b) (-> f a -> f b))
  -> (a -> b) -> f' (f a) -> f' (f b)


  f . g = \x-> f (g x)
  \x -> fmap (fmap x)
  -}

    it "another nested fmap example" $ do
      let dblFmap = (fmap . fmap )
            :: ([String] -> Char) -> [Maybe [String]] -> [Maybe Char]
      dblFmap rwp lmls `shouldBe` [ Just 'p', Nothing, Just 'p' ]
      let tripFmap = (fmap . fmap . fmap)
            :: (String -> Char) -> [Maybe [String]] -> [Maybe String]
      tripFmap rwp lmls `shouldBe` [ Just "pp", Nothing, Just "" ]
      let quadFmap = (fmap . fmap . fmap . fmap)
      quadFmap rwp lmls `shouldBe` [ Just ["pp", "pp"], Nothing, Just [] ]

    it "exercises: heaving lifting" $ do
      -- a
      (fmap (+1) $ read "[1]" :: [Int]) `shouldBe` [2]
      -- b
      (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
        `shouldBe` Just ["Hi,lol", "Hellolol"]
      -- c
      ((*2) . (\x -> x - 2) $ 1) `shouldBe` (-2)
      -- d
      (((return '1' ++) . show)
        ((\x -> [x, 1..3]) 0)) `shouldBe` "1[0,1,2,3]"
      -- e
      ioi <- readIO "1" :: IO Integer
      let changed = read $ ("123"++) (show ioi)
      (*3) changed `shouldBe` 3693
    it "quickcheck example identity" $ do
      property (functorIdentity :: [Int] -> Bool)
    it "quickcheck example compose" $ do
      property ((functorCompose (+1) (*2)) :: [Int] -> Bool)
    it "quickcheck example compose w/ coarbitrary " $ do
      property (functorCompose'  :: Fun Int Int -> Fun Int Int -> [Int] -> Bool)
    describe "exercises: instances of func" $ do
      describe "identity" $ do
        it "id" $ do
          property (functorIdentity :: Identity Int -> Bool)
        it "compose" $ do
          property (functorCompose' :: Fun Int Int -> Fun Int Int -> Identity Int -> Bool)

      describe "pair" $ do
        it "id" $ do
          property (functorIdentity :: Pair Int -> Bool)
        it "compose" $ do
          property (functorCompose' :: Fun Int Int -> Fun Int Int -> Pair Int -> Bool)
