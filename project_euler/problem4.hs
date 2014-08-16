module Problem4 where

{-
(1,1)(1,2)(1,3)
(2,1)(2,2)(2,3)
(3,1)(3,2)
-}



nextTuple (a,b)
   | a == 1 = (b+1, 1)
nextTuple (a,b) = (a-1,b+1)

nextMult (a,b) = (999-a)*(999-b)

isPalindrome num = iter 0 num
  where iter at 0 = at == num
        iter at 1 = at == num
        iter at goal = iter (at*10+new_num) new_goal 
          where new_num = goal `mod` 10
                new_goal = goal `div` 10

problem4 num = iter (1,1)  
  where iter at = if (isPalindrome $ nextMult at) then (nextMult at) else iter $ nextTuple at
