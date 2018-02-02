module Chapter08 where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord n = error $ "must use integers within range 0-9, got " ++ show n

digits :: Int -> [Int]
digits n =
  reverse $ digits' n
  where
    digits' 0 = []
    digits' n =
      ones : digits' tens
      where
        ones = n `rem` 10
        tens = n `div` 10


wordNumber :: Int -> String
wordNumber n =
  let
    digitsList = digits n
    inWords = map digitToWord digitsList
    hyphenSeperated = intersperse "-" inWords
  in
    concat hyphenSeperated
