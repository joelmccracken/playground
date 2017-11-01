> module Chapter09 (test) where

"Ciphers"

I actually wrote this at the end of chapter 11, but it belongs here, soooo, putting it here for now/as a better organization method

> import Data.Char
> import qualified TestLib

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
>     TestLib.testTrue "Caesar Cipher" isCorrect


"Writing your own standard functions"

> myOr :: [Bool] -> Bool
> myOr []     = False
> myOr (x:xs) = x || myOr xs

> testMyOr = do
>   TestLib.testTrue "myOr empty" $ not $ myOr []
>   TestLib.testTrue "myOr" $ myOr [False, False, False, True, False]
>   TestLib.testTrue "myOr not bottom on undefined" $ myOr [True, undefined]

> myAny :: (a -> Bool) -> [a] -> Bool
> myAny f []     = False
> myAny f (x:xs) = f x || myAny f xs

> testMyAny = do
>   TestLib.testTrue "myAny empty" $ not $ myAny (const True) []
>   TestLib.testTrue "myAny" $ myAny (== True) [False, False, False, True, False]
>   TestLib.testTrue "myAny not bottom on undefined" $ myAny (== True) [True, undefined]
>   TestLib.testTrue "myAny even" $ not $ myAny even [1, 3, 5]
>   TestLib.testTrue "myAny odd" $ myAny odd [1, 3, 5]

> myElem :: Eq a => a -> [a] -> Bool
> myElem a [] = False
> myElem a (x:xs) = (a == x) || myElem a xs

> testMyElem = do
>   TestLib.testTrue "myElem true" $ myElem 1 [1..10]
>   TestLib.testTrue "myElem false" $ not $ myElem 1 [2..10]


> myElemViaAny :: Eq a => a -> [a] -> Bool
> myElemViaAny a = myAny (== a)

> testMyElemViaAny = do
>   TestLib.testTrue "myElemViaEny true" $ myElemViaAny 1 [1..10]
>   TestLib.testTrue "myElemViaEny false" $ not $ myElemViaAny 1 [2..10]

> myReverse :: [a] -> [a]
> myReverse []     = []
> myReverse (x:xs) = myReverse xs ++ [x]

> testMyReverse = do
>   TestLib.testTrue "myReverse" $ myReverse [1..5] == [5,4,3,2,1]


> squish :: [[a]] -> [a]
> squish [] = []
> squish (x:[]) = x
> squish (x:xs) = x ++ squish xs

> testSquish = do
>   TestLib.testTrue "squish" $ squish [[1,2], [3], [5]] == [1,2,3,5]

> squishMap :: (a -> [b]) -> [a] -> [b]
> squishMap f [] = []
> squishMap f (x:[]) = f x
> squishMap f (x:xs) = f x ++ squishMap f xs

> testSquishMap = do
>   TestLib.testTrue "squishMap" $ squishMap (\x->[1,x]) [1,2,3,5] == [1,1,1,2,1,3,1,5]


> squishAgain :: [[a]] -> [a]
> squishAgain = squishMap id

> testSquishAgain = do
>   TestLib.testTrue "squishAgain" $ squishAgain [[1,2], [3], [5]] == [1,2,3,5]

> myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
> myMaximumBy f [] = error "no max to an empty list bro"
> myMaximumBy f (x:[]) = x
> myMaximumBy f (x:y:zs) =
>   case (f x y) of
>     GT -> myMaximumBy f (x:zs)
>     _  -> myMaximumBy f (y:zs)

> testMyMaximumBy = do
>   TestLib.testTrue "myMaximumBy" $ myMaximumBy compare [1, 53, 9001, 10] == 9001

> myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
> myMinimumBy f [] = error "no min to an empty list bro"
> myMinimumBy f (x:[]) = x
> myMinimumBy f (x:y:zs) =
>   case (f x y) of
>     LT -> myMinimumBy f (x:zs)
>     _  -> myMinimumBy f (y:zs)

> testMyMinimumBy = do
>   TestLib.testTrue "myMinimumBy" $ myMinimumBy compare [1, 53, 9001, 10] == 1



> myMaximum :: (Ord a) => [a] -> a
> myMaximum = myMaximumBy compare

> myMinimum :: (Ord a) => [a] -> a
> myMinimum = myMinimumBy compare

> test :: IO ()
> test = do
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
