import Control.Exception


-- Exercise 1

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
  assert
 ((toDigits 1234) == [1,2,3,4]) True    &&
  assert
 ((toDigitsRev 1234) == [4,3,2,1]) True &&
  assert
 ((toDigits 0) == []) True              &&
  assert
 ((toDigits (-17)) == []) True



-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOtherInt $ reverse xs
  where doubleEveryOtherInt (x1:x2:xs) = x1 : (2 * x2) : doubleEveryOtherInt(xs)
        doubleEveryOtherInt xs = xs


doubleEveryOtherAssertions :: Bool
doubleEveryOtherAssertions =
  assert
 ((doubleEveryOther [1,2,3,4]) == [2,2,6,4]) True   &&
  assert
 ((doubleEveryOther [8,7,6,5]) == [16,7,12,5]) True &&
  assert
 ((doubleEveryOther [1,2,3])   == [1,4,3]) True     &&
  assert
 ((doubleEveryOther [])        == []) True


-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concatMap toDigits xs


sumDigitsAssertions :: Bool
sumDigitsAssertions =
  assert
 ((sumDigits [16,7,12,5]) == 22) True


-- Exercise 4

validate :: Integer -> Bool
validate x = ((sumDigits $ doubleEveryOther $ toDigits x) `mod` 10) == 0

validateAssertions =
  assert ((validate 4012888888881881) == True) True   &&
  assert ((validate 4012888888881882) == False) True

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n start goal temp =
  hanoi (n - 1) start temp goal ++
  [(start, goal)] ++
  hanoi (n - 1) temp goal start

hanoiAssertions =
  assert (hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]) True &&
  assert ((length (hanoi 15 "a" "b" "c")) == 32767) True

-- Exercise 6

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n start goal temp1 temp2 =
  hanoi4 topHalf start temp1 goal temp2 ++
  hanoi bottomHalf start temp2 goal ++
  [(start, goal)] ++
  hanoi bottomHalf temp2 goal start ++
  hanoi4 topHalf temp1 goal start temp2
  where
    remaining = n - 1
    topHalf = remaining `div` 2
    bottomHalf = remaining - topHalf

h4 n = hanoi4 n "a" "b" "c" "d"



type HanoiStateVis = [[Integer]]
data VisItem = HanoiStateVis | Move

h4v :: Integer -> [Move] -> [VisItem]
h4v n states = vis [[1..n], [], [], []] states

vis :: HanoiStateVis -> [Move] -> [VisItem]
vis _ []        = []
vis state (move:moves) =
  [state, move, newState] ++ vis newState moves
  where
    newState = nextState state move

nextState :: HanoiStateVis -> Move -> HanoiStateVis
nextState ((a:as):(b:bs):(c:cs):(d:ds):[]) (from, to) =
  let newas = newStateInt "a" a as
      newbs = newStateInt "b" b bs
      newcs = newStateInt "c" c cs
      newds = newStateInt "d" d ds
  in
    [newas, newbs, newcs, newds]
  where
    moved =
      case from of
        "a" -> a
        "b" -> b
        "c" -> c
        "d" -> d
    newStateInt :: String -> Integer -> [Integer] -> [Integer]
    newStateInt col x xs =
      case col of
        from -> xs
        to   -> (moved : x : xs)
        _    -> (x:xs)

-- hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
-- hanoi4 0 _ _ _ _ = []
-- hanoi4 1 start goal _ _ = [(start, goal)]
-- hanoi4 n start goal temp1 temp2 =
--   hanoi4 (n-2) start temp1 goal temp2 ++
--   [(start, temp2)] ++
--   [(start, goal)] ++
--   [(temp2, goal)] ++
--   hanoi4 (n-2) temp1 goal temp2 start

hanoi4Assertions =
  assert ((length (hanoi4 15 "a" "b" "c" "d")) == 129) True

first  (x:_)       = x
second (_:x:_)     = x
third  (_:_:x:_)   = x
fourth (_:_:_:x:_) = x

type HanoiState = [[Integer]]

main :: IO ()
main = do
  print $ hanoi4 5 "a" "b" "c" "d"
  print $ length $ hanoi4 15 "a" "b" "c" "d"

-- main :: IO ()
-- main = do
--   putStrLn "toDigitsAssertions..."
--   print toDigitsAssertions

--   putStrLn "doubleEveryOtherAssertions..."
--   print doubleEveryOtherAssertions

--   putStrLn "sumDigitsAssertions..."
--   print sumDigitsAssertions

--   putStrLn "validateAssertions..."
--   print validateAssertions

--   putStrLn "hanoiAssertions..."
--   print hanoiAssertions

--   putStrLn "hanoi4Assertions..."

--   print $ length $ hanoi4 15 "a" "b" "c" "d"

--   print hanoi4Assertions
