module Chapter22 where

import Control.Applicative
import Data.Char
import Test.Hspec

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

main :: IO ()
main = hspec $ do
  it "warming up" $ do
    composed "Julie" `shouldBe` "EILUJ"
    fmapped  "Julie" `shouldBe` "EILUJ"
    tupled   "Julie" `shouldBe` ("eiluJ", "JULIE")
    tupled2  "Julie" `shouldBe` ("eiluJ", "JULIE")
