{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

import Control.Applicative
import Data.Char
import Test.Hspec
-- import Control.Monad.Reader

import Test.QuickCheck.Function
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop
bip2 = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: String -> (String, String)
tupled = (,) <$> rev <*> cap

tupled2 :: String -> (String, String)
tupled2 = do
  a <- rev
  b <- cap
  return (a,b)

tupled3 :: String -> (String, String)
tupled3 = cap >>= (rev >>= return (,))

newtype Reader r a
  = Reader { runReader :: r -> a}

-- exercise: ask
ask :: Reader a a
ask = Reader id

bindR :: (r -> a) -> (a -> r -> b) -> (r -> b)
bindR = (>>=)

newtype HumanName
  = HumanName String
  deriving (Eq, Show)

newtype DogName
  = DogName String
  deriving (Eq, Show)

newtype Address
  = Address String
  deriving (Eq, Show)

data Person =
  Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

-- exercise: reading comprehension

-- 1.
myLiftA2 :: Applicative f
         => (a -> b -> c)
         -> f a
         -> f b
         -> f c
myLiftA2 g fa fb =
  g <$> fa <*> fb

myLiftA2' :: (a -> b -> c)
          -> (r -> a)
          -> (r -> b)
          -> (r -> c)
myLiftA2' = myLiftA2

-- 2.

asks :: (r -> a) -> Reader r a
asks f = Reader f

-- 3.

instance Functor (Reader r) where
  fmap :: (a -> b)
       -> Reader r a
       -> Reader r b
  fmap f (Reader ra) =
    Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> (rab r) (ra r)

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r

main :: IO ()
main = hspec $ do
  it "warming up" $ do
    composed "Julie" `shouldBe` "EILUJ"
    fmapped  "Julie" `shouldBe` "EILUJ"
    tupled   "Julie" `shouldBe` ("eiluJ", "JULIE")
    tupled2  "Julie" `shouldBe` ("eiluJ", "JULIE")

  it "my lift a2" $ do
    let g = (+)
    let fa = length :: String -> Int
    let fb = length :: String -> Int
    (myLiftA2' g fa fb) "hi there" `shouldBe` (liftA2 g fa fb) "hi there"

  it "testing reader applicative" $ do
    let x = pure (+) :: Reader a (Int -> Int -> Int)
    let y = pure 1 :: Reader a Int
    let z = pure 2 :: Reader a Int
    (runReader x) 'a' 1 2 `shouldBe` 3
    (runReader (x <*> y)) 'x' 3 `shouldBe` 4
    (runReader (x <*> y <*> z)) 'x' `shouldBe` 3

  it "testing reader monad" $ do
    let ra = asks (+5)
    (runReader (ra >>= \a -> asks (a*))) 2 `shouldBe` 14
