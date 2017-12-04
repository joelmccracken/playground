module Chapter10 where

import qualified TestLib
import Data.Time
import Data.Bool
import Control.Monad


t str = TestLib.testTrue ("Chapter10: " ++ str)

testUnderstaindFoldLR = do
  t "foldLeftOrder" $ "f(f(f(0,1),2),3)" == ( foldl (\x y->"f(" ++ x ++ "," ++ show y ++ ")") "0" [1,2,3])
  t "foldRightOrder" $ "f(1,f(2,f(3,0)))" == ( foldr (\x y->"f(" ++ show x ++ "," ++ y ++ ")") "0" [1,2,3] )


{-
exercises: understanding folds
1. b and c with both return the same as the expr in question

2.
  foldl (flip (*)) 1 [1..3]
  foldl (*) 1 [1..3]
  foldl (*) (1 * 1) [2,3]
  foldl (*) ((1 * 1) * 2) [3]
  foldl (*) (((1 * 1) * 2) * 3) []
  (((1 * 1) * 2) * 3)
  6
3. c, associates to the right
4. a, reduce structure

5. a. foldr (++) "" ["woot", "WOOT", "woot"]
   b. foldr max 'a' ("fear is the little death" :: String)
   c. foldr (&&) True [False, True]
   d. foldr (||) False [False, True] though not sure it is "wrong", just doesn't make much sense
   e. foldr ((++) . show) "" [1..5]
   f. foldr const 0 [1..5] ... I think. Not sure what intended behavior is here
   g. foldr const 'a' ("tacos" :: String) . same as before, wrong type of acc.
   h. foldl (flip const) 'a' ("burritos" :: String). wrong type of acc
   i. foldl (flip const) 0 [1..5] . same as before... why are these all the same...
-}



-- Exercises: Database Processing

data DatabaseItem a = DbString String
                    | DbNumber a
                    | DbDate   UTCTime
   deriving (Eq, Ord, Show)

dbTime1 = (UTCTime
            (fromGregorian 1931 5 1)
            (secondsToDiffTime 34123))

dbTime2 = (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))

theDatabase :: Num a => [DatabaseItem a]
theDatabase =
  [ DbDate dbTime1
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate dbTime2
  , DbNumber 20
  ]

-- 1

filterDbDate :: [DatabaseItem a] -> [UTCTime]
filterDbDate = foldr onlyDates []
  where
    onlyDates :: DatabaseItem a -> [UTCTime] -> [UTCTime]
    onlyDates (DbDate date) dates = date : dates
    onlyDates _ dates = dates

testFilterDbDate = t "filterDbDate" $ filterDbDate theDatabase == [dbTime1, dbTime2]

-- 2

filterDbNumber :: Num a => [DatabaseItem a] -> [a]
filterDbNumber = foldr onlyNumbers []
  where
    onlyNumbers :: Num a => DatabaseItem a -> [a] -> [a]
    onlyNumbers (DbNumber number) numbers = number : numbers
    onlyNumbers _ numbers = numbers

testFilterDbNumber = t "filterDbNumber" $ filterDbNumber theDatabase == [9001, 20]

-- 3

mostRecent :: [DatabaseItem a] -> UTCTime
mostRecent = maximum . filterDbDate

testMostRecent = t "mostRecent" $ mostRecent theDatabase == dbTime1

-- 4

sumDb :: Num a => [DatabaseItem a] -> a
sumDb = sum . filterDbNumber

testSumDb = t "sumDb" $ sumDb theDatabase == 9021

-- 5

avgDb :: (Integral a, Fractional b) => [DatabaseItem a] -> b
avgDb db =
  let
    onlyNums = map fromIntegral $ filterDbNumber db
    numsTotal = sum onlyNums
    numNums = (fromIntegral . length) onlyNums
  in
    numsTotal / numNums

testAvgDb = t "avgDb" $ avgDb theDatabase == 4510.5

-- scans

fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 fibs

testFibs20 = t "fibs20" $ fibs20 == [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]

fibsUnder100 = takeWhile (<100) fibs

testfibsUnder100 = t "fibsUnder100" $ fibsUnder100 == [1,1,2,3,5,8,13,21,34,55,89]

facsScan = scanl (*) 1 [2..]
testFacsScan  = t "facsScan" $ take 5 facsScan == [1,2,6,24,120]

-- standard functions with folds

myOr :: [Bool] -> Bool
myOr = foldr (||) False
testMyOr = do
  t "myOr" $ myOr [False, True] == True
  t "myOr2" $ myOr [False] == False

myAny :: (a -> Bool) -> [a] -> Bool
myAny g = foldr ((||) . g) False

testMyAny = do
  t "myAny"  $ myAny even [1,3,5] == False
  t "myAny2" $ myAny odd [1,3,5] == True


myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==x)) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 = myAny . (==)

testMyElem = do
  t "myElem False"  $ myElem 4 [1,3,5] == False
  t "myElem True" $ myElem 3 [1,3,5] == True
  t "myElem2 False"  $ myElem2 4 [1,3,5] == False
  t "myElem2 True" $ myElem2 3 [1,3,5] == True

myReverse :: [a] -> [a]
myReverse = foldr ((flip (++)) . (:[])) []
testMyReverse = do
  t "myReverse" $ myReverse [1..5] == [5,4,3,2,1]

myMap :: (a -> b) -> [a] -> [b]
myMap = (flip foldr) [] . ((:) .)

testMyMap = do
  t "myMap" $ myMap (+1) [4, 6, 8] == [5,7,9]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = (flip foldr) [] . (ap (bool id . (:)))

testMyFilter = do
  t "myFilter" $ myFilter even [4, 5, 8] == [4,8]

squish :: [[a]] -> [a]
squish = foldr (++) []

testSquish = do
  t "squish" $ squish [[4, 5], [8]] == [4,5,8]

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = (flip foldr) [] . ((++) .)

testSquishMap = do
  t "squishMap" $ squishMap (\x->[1,x] :: [Integer]) [4, 5, 8] == [1,4,1,5,1,8]

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

testSquishAgain = do
  t "squishAgain" $ squishAgain [[4, 5], [8]] == [4,5,8]

maxBy f x y =
  case f x y of
    GT -> x
    _  -> y

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (maxBy f) x xs

testMyMaximumBy = do
  t "myMaximumBy 1" $ myMaximumBy (\_ _ -> GT) [1..10]  == 1
  t "myMaximumBy 2" $ myMaximumBy (\_ _ -> LT) [1..10] == 10
  t "myMaximumBy 3" $ myMaximumBy compare [1..10] == 10

minBy f x y =
  bool x y ((maxBy f x y) == x)

myMinimumBy :: Eq a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (minBy f) x xs

testMyMinimumBy = do
  t "myMinimumBy 1" $ myMinimumBy (\_ _ -> GT) [1..10]  == 10
  t "myMinimumBy 2" $ myMinimumBy (\_ _ -> LT) [1..10] == 1
  t "myMinimumBy 3" $ myMinimumBy compare [1..10] == 1



test = do
  testUnderstaindFoldLR
  testFilterDbDate
  testFilterDbNumber
  testMostRecent
  testSumDb
  testAvgDb
  testFibs20
  testfibsUnder100
  testFacsScan
  testMyOr
  testMyAny
  testMyElem
  testMyReverse
  testMyMap
  testMyFilter
  testSquish
  testSquishMap
  testSquishAgain
  testMyMaximumBy
  testMyMinimumBy
