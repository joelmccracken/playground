module Chapter27 where

import Test.Hspec
import qualified Chapter27StrictList as SL
-- exercises: evaluate


exercisesEvaluate = do
{-
1. const 1 undefined

const 1 undefined
(\x y->x) 1 undefined
(\y->1) undefined
1
does not bottom
-}
  it "const 1 undefined == 1" $ do
    const 1 undefined `shouldBe` (1 :: Int)

{-
2. const undefined 1
(\x y->x) undefined 1
(\y-> undefined) 1
undefined
-}
  it "const undefined 1 == undefined" $ do
    const undefined 1 `shouldThrow` anyException

{-
3. flip const undefined 1
(\f x y -> f y x) const undefined 1
(\x y -> const y x) undefined 1
(\y -> const y undefined) 1
const 1 undefined
from ex 1, == 1
-}
  it "flip const undefined 1 == 1" $ do
    flip const undefined 1  `shouldBe` (1 :: Int)

{-
4. flip const 1 undefined
(\f x y -> f y x) const 1 undefined
(\x y -> const y x) 1 undefined
(\y -> const y 1) undefined
const undefined 1
from ex 2, == undefined
-}
  it "flip const 1 undefined == undefined" $ do
    flip const 1 undefined `shouldThrow` anyException

{-
5. const undefined undefined
(\x y->x) undefined undefined
(\y-> undefined) undefined
undefined
-}
  it "const undefined undefined = undefined" $ do
    const undefined undefined `shouldThrow` anyException
{-
6. foldr const 'z' ['a'..'e']

(\f z xs ->
  case xs of
    [] ->z
    (x:xs) -> f x (foldr f z xs)) const 'z' ['a'..'e']
(\z xs ->
  case xs of
    [] -> z
    (x:xs) -> const x (foldr const z xs)) 'z' ['a'..'e']
(\xs ->
  case xs of
    [] -> 'z'
    (x:xs) -> const x (foldr const 'z' xs)) ['a'..'e']
case ['a'..'e'] of
  [] -> 'z'
  (x:xs) -> const x (foldr const 'z' xs)
const 'a' (foldr const 'z' ['b..e'])
const 'a' (foldr const 'z' ['b..e'])
'a'
-}
  it "foldr const 'z' ['a'..'e'] == 'a'" $ do
    foldr const 'z' ['a'..'e'] `shouldBe` 'a'

{-
7. foldr (flip const) 'z' ['a'..'e']
(\f z xs ->
  case xs of
    [] ->z
    (x:xs) ->  f x (foldr f z xs)) (flip const) 'z' ['a'..'e']
(\z xs ->
  case xs of
    [] ->z
    (x:xs) ->  (flip const) x (foldr (flip const) z xs)) 'z' ['a'..'e']
(\xs ->
  case xs of
    [] -> 'z'
    (x:xs) ->  (flip const) x (foldr (flip const) 'z' xs)) ['a'..'e']
case ['a'..'e'] of
  [] -> 'z'
  (x:xs) ->  (flip const) x (foldr (flip const) 'z' xs)

(flip const) 'a' (foldr (flip const) 'z' ['b'..'e'])
(\x y -> const y x) 'a' (foldr (flip const) 'z' ['b'..'e'])
(\y -> const y 'a') (foldr (flip const) 'z' ['b'..'e'])
const (foldr (flip const) 'z' ['b'..'e']) 'a'
foldr (flip const) 'z' ['b'..'e']
... 'z'
-}
  it "foldr (flip const) 'z' ['a'..'e'] == 'z'" $ do
   foldr (flip const) 'z' ['a'..'e']  `shouldBe` 'z'


{-

-}

main :: IO ()
main = do
  hspec $ do
    exercisesEvaluate
  SL.main
