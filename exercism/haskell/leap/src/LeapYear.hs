module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year =
  (evenlyDivisibleBy year 4) &&
  (not (evenlyDivisibleBy year 100)) ||
  (evenlyDivisibleBy year 400)
  where
    evenlyDivisibleBy x y = (x `mod` y) == 0
