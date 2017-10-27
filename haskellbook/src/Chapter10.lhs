> module Chapter10 where


I actually wrote this at the end of chapter 11, but it belongs here, soooo

> import Data.Char

> shiftChar :: Int -> Char -> Char
> shiftChar shiftNum char =
>   if isAlpha char then
>     if isLower char then
>       shiftChar' shiftNum char 'a' 'z'
>     else
>       shiftChar' shiftNum char 'A' 'Z'
>   else
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
