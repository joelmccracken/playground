


abc's = [(a,b,c) | c <- [1..1000], b <- [1..(c-1)], a <- [1..(b-1)]]


sq :: Integer -> Integer
sq n = n*n


eq1k (a,b,c) = a+b+c==1000

isTriplet (a,b,c) = (sq a) + (sq b) == (sq c) 

problem9 (x:xs) = if ((isTriplet x) && (eq1k x)) then x else  problem9 xs

main = do
  print . (\(x,y,z)->x*y*z) $ problem9 abc's
 
