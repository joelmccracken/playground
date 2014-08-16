a (m, n)
  | m == 0 = n+1
  | m > 0 && n == 0 = a ((m-1),1)
  | m > 0 && n > 0 = a ((m-1), (a (m,(n-1)))) 

ack_pairs n m = [(i,j) | i <- [0..n], j <- [0..m]]

ack :: Integer -> Integer -> [Integer]
ack n m = map a (ack_pairs n m)

