module Chapter12 where

import qualified TestLib
import qualified Chapter09 as C9
import Data.Char
import Data.List
import Data.Maybe
import Data.Function ((&))

t str = TestLib.testTrue ("Chapter12: " ++ str)

-- determine the kinds
-- 1. a is *
-- 2. a is *, f is * -> *


-- String Processing

-- 1.
notThe :: String -> Maybe String
notThe str =
  case (str == "the") of
    True  -> Nothing
    False -> Just str

replaceThe :: String -> String
replaceThe str = intercalate " " $ map (fromMaybe "a") $ (map notThe $ C9.myWords str)

testThe = do
  t "nt1" $ notThe "the" == Nothing
  t "nt2" $ notThe "blahtheblah" == Just "blahtheblah"
  t "nt3" $ notThe "blahtheblah" == Just "blahtheblah"
  t "nt4" $ notThe "woot" == Just "woot"
  t "nt5" $ replaceThe "the cow loves us" == "a cow loves us"

-- 2.

isVowel :: Char -> Bool
isVowel = flip C9.myElem $ "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel input =
  input & C9.myWords & countHelper 0
  where
    countHelper n (a : bb@(b : bs) : xs) =
      if a == "the" && isVowel b then
        countHelper (n+1) xs
      else
        countHelper n (bb:xs)
    countHelper n _ = n


testCountVowel = do
 t "cbv" $ countTheBeforeVowel "the cow" == 0
 t "cbv2" $ countTheBeforeVowel "the evil cow" == 1

runTests :: IO ()
runTests = do
  testThe
  testCountVowel
