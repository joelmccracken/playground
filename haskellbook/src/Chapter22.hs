module Chapter22 where

import Control.Applicative
import Data.Char
import Test.Hspec
-- import Control.Monad.Reader


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


-- exercise: reading comprehension

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

main :: IO ()
main = hspec $ do
  it "warming up" $ do
    composed "Julie" `shouldBe` "EILUJ"
    fmapped  "Julie" `shouldBe` "EILUJ"
    tupled   "Julie" `shouldBe` ("eiluJ", "JULIE")
    tupled2  "Julie" `shouldBe` ("eiluJ", "JULIE")

  it "my lift a2" $ do
    let g = (+)
    let fa = length
    let fb = length
    (myLiftA2' g fa fb) "hi there" `shouldBe` (liftA2 g fa fb) "hi there"
