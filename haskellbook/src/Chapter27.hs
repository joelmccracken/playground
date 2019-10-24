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
What will :sprint output?


1. let x = 1
should be x = _, because x is polymorphic
actual x = _

2. let x = ['1']
should be x = "1", i think; weak head normal form?
actual x = "1"

3. let x = [1]
since type needs to be applied (num), i *think* it will be x = [_]
actual x = [_] hmm; i made the mistake of thinking that the list would eval to
the point where the num constraint is needed; P1060 in RC4-screen shows the
correct behavior.
in core x is a lambda expr, needing to be applied.

4. let x = 1 :: Int
should be x = 1
actual x = 1


5. let f = \x -> x
   let x = f 1
should be  f = _, x = _ since f is a function and x is a fn appl
is correct


6. let f :: Int -> Int; f = \x -> x
   let x = f 1
same as above, f is still a function
correct

-}


{-

Will printing this expression result in bottom?

1. no; undefined is not evaluated
2. yes; evaluating y will evaluate x first
3. yes; in order to calc length it must walk the length of the spine, evaluating undefined
4. no; undefined neednt be evaluated to calculate the length of the list
5. no; no need to evaluate the undefined
6. no; the 1s are not shared
7. yes, const evaluates to undefined, sooo...
-}


makeExpressionBottom = do
  it "bottoms w/ seq" $ do
    let x = undefined
    let y = "blah"
    let main = x `seq` (print (snd (x, y)))
    let main' = (print $ seq x (snd (x, y)))
    let main'' = (print (snd (x, x `seq` y)))
    main `shouldThrow` anyException
    main' `shouldThrow` anyException
    main'' `shouldThrow` anyException



main :: IO ()
main = do
  hspec $ do
    exercisesEvaluate
    makeExpressionBottom
  SL.main
  putStrLn "fin 27."
