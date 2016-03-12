import Control.Exception

toDigits :: Integer -> [Integer]

toDigits 0 = []
toDigits n
  | n < 0  = []
  | otherwise = toDigitsInt n []
     where toDigitsInt n acc
             | n > 0        = (toDigitsInt _next  (_rem : acc))
             | otherwise    = acc
               where _rem  = (n `mod` 10)
                     _next = (n `div` 10)

toDigitsRev = reverse . toDigits

toDigitsAssertions :: Bool
toDigitsAssertions =
  assert ((toDigits 1234) == [1,2,3,4]) True    &&
  assert ((toDigitsRev 1234) == [4,3,2,1]) True &&
  assert ((toDigits 0) == []) True              &&
  assert ((toDigits (-17)) == []) True


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOtherInt $ reverse xs
  where doubleEveryOtherInt (x1:x2:xs) = x1 : (2 * x2) : doubleEveryOtherInt(xs)
        doubleEveryOtherInt xs = xs


doubleEveryOtherAssertions :: Bool
doubleEveryOtherAssertions =
  assert ((doubleEveryOther [1,2,3,4]) == [2,2,6,4]) True   &&
  assert ((doubleEveryOther [8,7,6,5]) == [16,7,12,5]) True &&
  assert ((doubleEveryOther [1,2,3])   == [1,4,3]) True     &&
  assert ((doubleEveryOther [])        == []) True


sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concatMap toDigits xs


sumDigitsAssertions :: Bool
sumDigitsAssertions =
  assert ((sumDigits [16,7,12,5]) == 22) True


validate :: Integer -> Bool
validate x = ((sumDigits $ doubleEveryOther $ toDigits x) `mod` 10) == 0

validateAssertions =
  assert ((validate 4012888888881881) == True) True   &&
  assert ((validate 4012888888881882) == False) True

main :: IO ()
main = do
  putStrLn "toDigitsAssertions..."
  print toDigitsAssertions

  putStrLn "doubleEveryOtherAssertions..."
  print doubleEveryOtherAssertions

  putStrLn "sumDigitsAssertions..."
  print sumDigitsAssertions

  putStrLn "validateAssertions..."
  print validateAssertions
