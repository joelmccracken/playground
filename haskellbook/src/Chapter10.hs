module Chapter10 where

import qualified TestLib

t str = TestLib.testTrue ("Chapter10: " ++ str)

testUnderstaindFoldLR = do
  t "foldLeftOrder" $ "f(f(f(0,1),2),3)" == ( foldl (\x y->"f(" ++ x ++ "," ++ show y ++ ")") "0" [1,2,3])
  t "foldRightOrder" $ "f(1,f(2,f(3,0)))" == ( foldr (\x y->"f(" ++ show x ++ "," ++ y ++ ")") "0" [1,2,3] )


{-
exercises: understanding folds
1. b and c with both return the same as the expr in question

2.
  foldl (flip (*)) 1 [1..3]
  foldl (*) 1 [1..3]
  foldl (*) (1 * 1) [2,3]
  foldl (*) ((1 * 1) * 2) [3]
  foldl (*) (((1 * 1) * 2) * 3) []
  (((1 * 1) * 2) * 3)
  6
3. c, associates to the right
4. a, reduce structure

5. a. foldr (++) "" ["woot", "WOOT", "woot"]
   b. foldr max 'a' ("fear is the little death" :: String)
   c. foldr (&&) True [False, True]
   d. foldr (||) False [False, True] though not sure it is "wrong", just doesn't make much sense
   e. foldr ((++) . show) "" [1..5]
   f. foldr const 0 [1..5] ... I think. Not sure what intended behavior is here
   g. foldr const 'a' ("tacos" :: String) . same as before, wrong type of acc.
   h. foldl (flip const) 'a' ("burritos" :: String). wrong type of acc
   i. foldl (flip const) 0 [1..5] . same as before... why are these all the same...
-}



test = do
  testUnderstaindFoldLR
