
divisible :: Int -> Int -> Bool
divisible x y = x `mod` y == 0


d3or5 :: Int -> Bool
d3or5 x = (divisible x 3) || (divisible x 5)

main :: IO ()
main = do 
  putStrLn $ show $ sum (filter d3or5 [1..999])

