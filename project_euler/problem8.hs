import IO
import List
import Data.Function

multString :: String -> Integer
multString = foldl (*) 1 . map (\x->(read [x]::Integer))


main = do x <- readFile "problem8.txt"
          print . foldl (\x y->if fst x > fst y then x else y) (1,"1") . map (\x->(multString x, x)) . filter (\x->length x == 5) . map (take 5) . tails .foldl (++) "" $ words x
