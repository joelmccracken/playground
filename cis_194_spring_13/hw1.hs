toDigits :: Integer -> [Integer]



toDigits n
  | n == 0 = []
  | n < 0  = []
  | otherwise = toDigitsInt n []
     where toDigitsInt n acc
             | (n `div` 10) > 0 = (toDigitsInt (n `div` 10)  (acc ++ [n `mod` 10]))
             | otherwise    = acc ++ [n]

toDigitsRev = reverse . toDigits

main :: IO ()
main = do
  putStrLn $ show (toDigits 1234)
  putStrLn $ show (toDigits 0)
  putStrLn $ show (toDigitsRev 139)
