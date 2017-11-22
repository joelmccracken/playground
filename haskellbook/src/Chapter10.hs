module Chapter10 where

import qualified TestLib

t str = TestLib.testTrue ("Chapter10: " ++ str)

testUnderstaindFoldLR = do
  t "foldLeftOrder" $ "f(f(f(0,1),2),3)" == ( foldl (\x y->"f(" ++ x ++ "," ++ show y ++ ")") "0" [1,2,3])
  t "foldRightOrder" $ "f(1,f(2,f(3,0)))" == ( foldr (\x y->"f(" ++ show x ++ "," ++ y ++ ")") "0" [1,2,3] )

test = do
  testUnderstaindFoldLR
