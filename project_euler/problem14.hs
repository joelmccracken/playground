collatz 1 = 1
collatz x | even x = 1 + collatz (x `div` 2)
collatz x = 1 + collatz(3*x + 1) 


gotill = 999999

problem14 1000000 highest = highest 
problem14 at      highest = if (collatz at) > (collatz highest) then (problem14 (at+1) at) else (problem14 (at+1) highest)


main = do
  print $ problem14 1 1

{-
main = do
  print . foldl (\(a,b) (c,d)->if b>d then (a,b) else (b,d)) (0, 1) $ map (\x -> (x, collatz x)) [1..999999]
-}
