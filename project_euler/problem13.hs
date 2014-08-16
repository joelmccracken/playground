import IO
import List


getInteger :: String -> Integer
getInteger = read

main = do d <- readFile "problem13.txt"
          print . foldl (+) 0 . map getInteger $ words d
