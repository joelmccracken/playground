
nums :: [Int]
nums = [1..100]



square :: Int -> Int
square x = x*x


main :: IO ()
main = do
  putStrLn $ show $ (square  (sum nums)) - (sum (map square nums)) 
  

