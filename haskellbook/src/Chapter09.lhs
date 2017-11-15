> module Chapter09 where

> import Data.Char
> import qualified TestLib
> import Data.Bool (bool)

> t str = TestLib.testTrue ("Chapter09: " ++ str)

EnumFromTo exercises

> eftBool :: Bool -> Bool -> [Bool]
> eftBool False False = [False]
> eftBool False True = [False, True]
> eftBool True False = []
> eftBool True True = [True]

> testEftBool = do
>   t "eftBool ff" $ eftBool False False == [ False .. False ]
>   t "eftBool ft" $ eftBool False True == [ False .. True ]
>   t "eftBool tf" $ eftBool True False == [ True .. False ]
>   t "eftBool tt" $ eftBool True True == [ True .. True  ]


see a way to implement general version, given two typeclasses...

> eftRange :: (Enum a, Ord a) => a -> a -> [a]
> eftRange x y
>   | x > y = []
>   | x == y = [x]
>   | otherwise = x : eftRange (succ x) y

> eftOrd :: Ordering
>        -> Ordering
>        -> [Ordering]
> eftOrd = eftRange

> testEftOrd = do
>   t "eftOrd" $ eftOrd LT GT == [ LT .. GT ]
>   t "eftOrd" $ eftOrd GT EQ == [ GT .. EQ ]
>   t "eftOrd" $ eftOrd GT GT == [ GT .. GT ]

> eftInt :: Int -> Int -> [Int]
> eftInt = eftRange

> testEftInt = do
>   t "eftInt" $ eftInt 0 1 == [ 0, 1 ]
>   t "eftInt" $ eftInt 5 10 == [ 5, 6, 7, 8, 9, 10 ]
>   t "eftInt" $ eftInt 10 10 == [ 10 ]

> eftChar :: Char -> Char -> [Char]
> eftChar = eftRange

> testEftChar = do
>   t "eftChar" $ eftChar 'q' 's' == [ 'q', 'r', 's' ]
>   t "eftChar" $ eftChar 'z' 'z' == [ 'z' ]
>   t "eftChar" $ eftChar 'p' 'l' == [ ]


"Exercises: Thy Fearful Symmetry"

> trimLeadingSpaces = dropWhile (== ' ')
> firstWord = (takeWhile (/= ' ')) . trimLeadingSpaces
> remainingString = trimLeadingSpaces . (dropWhile ( /= ' ')) . trimLeadingSpaces

> myWords :: String -> [String]
> myWords "" = []
> myWords str =
>   let
>     trimLeadingSpaces = dropWhile (== ' ')
>     firstWord         = (takeWhile (/= ' ')) . trimLeadingSpaces
>     remainingString   = trimLeadingSpaces . (dropWhile ( /= ' ')) . trimLeadingSpaces
>     thisWord          = firstWord str
>   in
>     if thisWord == "" then
>       []
>     else
>       thisWord : myWords (remainingString str)

> testMyWords = do
>   t "myWords" $ myWords "     " == []
>   t "myWords" $ myWords "  hi   " == ["hi"]
>   t "myWords" $ myWords "  hi   zocoo" == ["hi", "zocoo"]



> myLines :: String -> [String]
> myLines "" = []
> myLines str =
>   let
>     trimLeadingLines  = dropWhile (== '\n')
>     firstLine         = (takeWhile (/= '\n')) . trimLeadingLines
>     remainingString   = trimLeadingLines . (dropWhile ( /= '\n')) . trimLeadingLines
>     thisLine          = firstLine str
>   in
>     if thisLine == "" then
>       []
>     else
>       thisLine : myLines (remainingString str)

> testMyLines = do
>   t "myLines" $ myLines " \n " == [" ", " "]
>   t "myLines" $ myLines "\n\nhi\n" == ["hi"]
>   t "myLines" $ myLines "\n\nhi\n\nzocoo" == ["hi", "zocoo"]

> mySplitAt :: (a -> Bool) -> [a] -> [[a]]
> mySplitAt _ [] = []
> mySplitAt isSplitPoint str =
>   let
>     trimLeadingSplitPoints  = dropWhile isSplitPoint
>     firstGroup              = (takeWhile (not . isSplitPoint)) . trimLeadingSplitPoints
>     remainingString         = (dropWhile (not . isSplitPoint)) . trimLeadingSplitPoints
>     thisGroup               = firstGroup str
>   in
>     if length thisGroup == 0 then
>       []
>     else
>       thisGroup : mySplitAt isSplitPoint (remainingString str)

> testMySplitAt = do
>   t "mySplitAt" $ mySplitAt (== ' ') " \n " == ["\n"]
>   t "mySplitAt" $ mySplitAt (== ' ') "face \nx " == ["face", "\nx"]



"comprehend thy lists"

first one is all even squares till 100

second one is list tuples (x,y),
ordered numerically least to greatest first by x and then by y.
on tuple for each: x's are perfect squares <50, y's are perfect squares s.t. 50 < y <= 100

third is the first 5 elements of the previous list; enumerated, that would be:
[(1,64), (1,81), (1,100), (4, 64), (4, 81)]


"Square Cube"

> mySqr = [x^2 | x <- [1..5]]
> myCube = [y^3 | y <- [1..5]]


> someTuples = [(x,y) | x <- mySqr, y <- myCube]
> someTuplesMaxed = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]
> someTuplesMaxedLen = length someTuplesMaxed

> testSquaresAndCubes = do
>   t "someTuplesMaxed" $ someTuplesMaxed == [(1,1), (1,8), (1, 27),
>                                             (4,1), (4,8), (4, 27),
>                                             (9,1), (9,8), (9, 27),
>                                             (16,1), (16,8), (16, 27),
>                                             (25,1), (25,8), (25, 27)]



1. [x^y | x <- [1..5], y <- [2, undefined]] == bottom
2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]] == [1]
3. sum [1, undefined, 3] == bottom
4. length [1, 2, undefined] == 3
5. length $ [1, 2, 3] ++ undefined == bottom
6. take 1 $ filter even [1, 2, 3, undefined] == [2]
7. take 1 $ filter even [1, 3, undefined] == bottom
8. take 1 $ filter odd [1, 3, undefined] == [1]
9. take 2 $ filter odd [1, 3, undefined] == [1,3]
10. take 3 $ filter odd [1, 3, undefined] == bottom



intermission: is it normal form?

not sure about these:

1. nf
2. whnf
3. neither
4. neither
5. neither
6. whnf
7. whnf



"Exercises: More bottoms"

1. take 1 $ map (+1) [undefined, 2, 3] == bottom
2. take 1 $ map (+1) [1, undefined, 3] == [2]
3. take 2 $ map (+1) [1, undefined, 3] == bottom
4. `itIsMystery xs = map (\x -> elem x "aeiou") xs`
   it tells you which characters are vowels, returning a list of booleans, True for vowels and False for consonants.
5. a. [1,4,9,16,25,36,49,64,81,100]
   b. [1, 10, 20]
   c. [15, 15, 15]
6. below:

> foldBoolReplacement = map (\x->bool (-x) x (x /= 3)) [1..10]
> testFoldBoolReplacement = do
>   t "foldBoolReplacement" $ foldBoolReplacement == [1,2,-3,4,5,6,7,8,9,10]


"Exercises: Filtering"

1.

> multiplesOfThreeTill = filter ((== 0) . ((flip rem) 3))
> testMultiplesOfThree = t "multiplesOfThreeTill" $ multiplesOfThreeTill [1..30] == [3,6,9,12,15,18,21,24,27,30]

2. `length . multiplesOfThreeTill` (is there more to this?? I just rewrote the above to at least make the list of integers variable)
3.

> articles = ["a", "an", "the"]
> nonArticles :: String -> [String]
> nonArticles = filter (not . ((flip elem) articles)) . myWords
> testNonArticles = t "nonArticles" $ nonArticles "the brown dog was a goof" == ["brown","dog","was","goof"]


""

"Ciphers"

I actually wrote this at the end of chapter 11, but it belongs here, soooo, putting it here for now/as a better organization method

> shiftChar :: Int -> Char -> Char
> shiftChar shiftNum char =
>   if isAlpha char then
>     if isLower char then
>       shiftChar' shiftNum char 'a' 'z'
>     else
>       shiftChar' shiftNum char 'A' 'Z'
>   else
>     -- only shift alphabet characters
>     char
>   where
>     shiftChar' shiftNum char minChar maxChar =
>       let
>         ordA        = ord minChar
>         ordZ        = ord maxChar
>         ordChar     = ord char
>         charOffset  = ordChar - ordA
>         -- max offset neeeds to be one more; otheriwse final z wraps to a, unintentionally.
>         maxOffset   = 1 + ordZ - ordA
>         shiftedOffset = (mod (charOffset + shiftNum) maxOffset)
>         charOrdShifted = ordA + shiftedOffset
>       in
>         chr charOrdShifted

> caesar :: Int -> String -> String
> caesar _ "" = ""
> caesar num (c:str) =
>   (shiftChar num c) : caesar num str

Tests for caesar cipher

> testCaesar :: IO ()
> testCaesar =
>   let
>     testStr = "! foo BarBazz 22 ?"
>     isCorrect = testStr == caesar (-10) (caesar 10 testStr)
>   in
>     t "Caesar Cipher" isCorrect


"Writing your own standard functions"

> myOr :: [Bool] -> Bool
> myOr []     = False
> myOr (x:xs) = x || myOr xs

> testMyOr = do
>   t "myOr empty" $ not $ myOr []
>   t "myOr" $ myOr [False, False, False, True, False]
>   t "myOr not bottom on undefined" $ myOr [True, undefined]

> myAny :: (a -> Bool) -> [a] -> Bool
> myAny f []     = False
> myAny f (x:xs) = f x || myAny f xs

> testMyAny = do
>   t "myAny empty" $ not $ myAny (const True) []
>   t "myAny" $ myAny (== True) [False, False, False, True, False]
>   t "myAny not bottom on undefined" $ myAny (== True) [True, undefined]
>   t "myAny even" $ not $ myAny even [1, 3, 5]
>   t "myAny odd" $ myAny odd [1, 3, 5]

> myElem :: Eq a => a -> [a] -> Bool
> myElem a [] = False
> myElem a (x:xs) = (a == x) || myElem a xs

> testMyElem = do
>   t "myElem true" $ myElem 1 [1..10]
>   t "myElem false" $ not $ myElem 1 [2..10]


> myElemViaAny :: Eq a => a -> [a] -> Bool
> myElemViaAny a = myAny (== a)

> testMyElemViaAny = do
>   t "myElemViaEny true" $ myElemViaAny 1 [1..10]
>   t "myElemViaEny false" $ not $ myElemViaAny 1 [2..10]

> myReverse :: [a] -> [a]
> myReverse []     = []
> myReverse (x:xs) = myReverse xs ++ [x]

> testMyReverse = do
>   t "myReverse" $ myReverse [1..5] == [5,4,3,2,1]


> squish :: [[a]] -> [a]
> squish [] = []
> squish (x:[]) = x
> squish (x:xs) = x ++ squish xs

> testSquish = do
>   t "squish" $ squish [[1,2], [3], [5]] == [1,2,3,5]

> squishMap :: (a -> [b]) -> [a] -> [b]
> squishMap f [] = []
> squishMap f (x:[]) = f x
> squishMap f (x:xs) = f x ++ squishMap f xs

> testSquishMap = do
>   t "squishMap" $ squishMap (\x->[1,x]) [1,2,3,5] == [1,1,1,2,1,3,1,5]


> squishAgain :: [[a]] -> [a]
> squishAgain = squishMap id

> testSquishAgain = do
>   t "squishAgain" $ squishAgain [[1,2], [3], [5]] == [1,2,3,5]

> myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
> myMaximumBy f [] = error "no max to an empty list bro"
> myMaximumBy f (x:[]) = x
> myMaximumBy f (x:y:zs) =
>   case (f x y) of
>     GT -> myMaximumBy f (x:zs)
>     _  -> myMaximumBy f (y:zs)

> testMyMaximumBy = do
>   t "myMaximumBy" $ myMaximumBy compare [1, 53, 9001, 10] == 9001

> myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
> myMinimumBy f [] = error "no min to an empty list bro"
> myMinimumBy f (x:[]) = x
> myMinimumBy f (x:y:zs) =
>   case (f x y) of
>     LT -> myMinimumBy f (x:zs)
>     _  -> myMinimumBy f (y:zs)

> testMyMinimumBy = do
>   t "myMinimumBy" $ myMinimumBy compare [1, 53, 9001, 10] == 1



> myMaximum :: (Ord a) => [a] -> a
> myMaximum = myMaximumBy compare

> myMinimum :: (Ord a) => [a] -> a
> myMinimum = myMinimumBy compare

> test :: IO ()
> test = do
>   testEftBool
>   testEftOrd
>   testEftInt
>   testEftChar
>   testMyWords
>   testMyLines
>   testMySplitAt
>   testSquaresAndCubes
>   testFoldBoolReplacement
>   testMultiplesOfThree
>   testNonArticles
>   testCaesar
>   testMyOr
>   testMyAny
>   testMyElem
>   testMyElemViaAny
>   testMyReverse
>   testSquish
>   testSquishMap
>   testSquishAgain
>   testMyMaximumBy
>   testMyMinimumBy
