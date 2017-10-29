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

> test :: IO ()
> test = do
>   testCaesar
