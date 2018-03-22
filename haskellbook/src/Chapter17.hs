module Chapter17 where

import Data.List (elemIndex)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary, arbitrary, frequency, elements)
import Test.QuickCheck.Function
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA3)

import Data.Monoid

f :: Int -> Maybe String
f x =
  lookup x [ (3, "hello")
           , (4, "julie")
           , (5, "kbai")
           ]

g :: Int -> Maybe String
g x =
  lookup x [ (7, "sup?")
           , (8, "chris")
           , (9, "aloha")
          ]

h :: Int -> Maybe Int
h z =
  lookup z [(2,3), (5,6), (7,8)]

m :: Int -> Maybe Int
m x =
  lookup x [(4,10), (8,13), (1,9001)]

-- Exercises: Lookups
-- making expressions typecheck

-- 1.

added :: Maybe Integer
added =
  (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.

x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

-- 4.

xs = [1,2,3]
ys = [4,5,6]

x1 :: Maybe Integer
x1 = lookup 3 $ zip xs ys

y1 :: Maybe Integer
y1 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x1 <*> y1)

--- Exercise: Identity instance

data Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a = Identity a
  Identity aToB <*> Identity a = Identity $ aToB a


-- exercise: Constant instance

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant (x <> y)


-- Exercise: list applicative


data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

fmap' :: (a -> b) -> List a -> List b -> List b
fmap' f Nil listEnding         = listEnding
fmap' f (Cons x xs) listEnding = Cons (f x) (fmap' f xs listEnding)

listApp :: List (a->b) -> List a -> List b
listApp Nil _ = Nil
listApp (Cons f fs) xs =
  fmap' f xs (listApp fs xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) = listApp

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    ls <- arbitrary
    frequency [ (1, return $ Nil)
              , (10, return $ Cons a ls)
              ]

instance Eq a => EqProp (List a) where (=-=) = eq

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' x (Cons a ls)
  | x > 0 = Cons a (take' (x - 1) ls)
  | otherwise = Nil

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

zipListApp :: ZipList' (a -> b) -> ZipList' a -> ZipList' b
zipListApp (ZipList' fs) (ZipList' xs) =
  ZipList' $ zipListApp' fs xs
  where
    zipListApp' Nil _ = Nil
    zipListApp' _ Nil = Nil
    zipListApp' (Cons f fs') (Cons x xs') =
      Cons (f x) (zipListApp' fs' xs')


repeating a = Cons a (repeating a)

instance Applicative ZipList' where
  pure a = ZipList' $ repeating a
  (<*>)  = zipListApp

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' a

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure e) = Failure e

validationAp :: Monoid e =>
                Validation e (a -> b) ->
                Validation e a ->
                Validation e b
validationAp (Success f) (Success x) = Success (f x)
validationAp (Failure e) (Failure e') = Failure (e <> e')
validationAp (Failure e) _ = Failure e
validationAp _ (Failure e) = Failure e

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure e, Success a]

instance Monoid e => Applicative (Validation e) where
  pure x = Success x
  (<*>) = validationAp

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq


main :: IO ()
main = hspec $ do
  it "Identity" $ do
    let xs = [1,2,3]
    let xs' = [9,9,9]
    let res = const <$> xs <*> xs'
    res `shouldBe` [1,1,1,2,2,2,3,3,3]
    let mkId = Identity
    let res' = const <$> mkId xs <*> mkId xs'
    res' `shouldBe` Identity [1,2,3]
  it "Constant" $ do
    let f = Constant (Sum 1)
    let g = Constant (Sum 2)
    let res = f <*> g
    res `shouldBe` Constant {getConstant = Sum {getSum = 3}}
    let res' = pure 1 :: Constant String Int
    res' `shouldBe` Constant {getConstant = ""}
  describe "fixer upper" $ do
    it "1. const" $ do
      const <$> Just "Hello" <*> pure "World" `shouldBe` Just "Hello"
    it "2. Just" $ do
      let res = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
      res `shouldBe` (Just (90, 10, "Tierness", [1,2,3]))
  it "combinations" $ do
    length combinations `shouldBe` 180
    take 2 combinations `shouldBe` [('p','a','p'), ('p','a','b')]

listApplicativeExercise =
  quickBatch $ applicative (undefined :: List (String, String, Int))

zipListApplicativeExercise =
  quickBatch $ applicative (undefined :: ZipList' (Int, String, Int))

validationApplicativeExercise =
  quickBatch $ applicative (undefined ::  Validation String (Int, String, Int))


-- chapter exercises

-- 1.

pureArr :: a -> [] a
pureArr = pure

apArr :: [] (a -> b) -> [] a -> [] b
apArr = (<*>)

-- 2.

pureIO :: a -> IO a
pureIO = pure

apIO :: IO (a -> b) -> IO a -> IO b
apIO = (<*>)

-- 3.

pureDouble :: Monoid a => a -> (a,a)
pureDouble = pure

apDouble :: Monoid a => (a, (a -> b)) -> (a, a) -> (a, b)
apDouble = (<*>)

-- 4.

pureFn :: Monoid e => a -> (->) e a
pureFn = pure

apFn :: Monoid e => (->) e (a -> b) -> (->) e a -> (->) e b
apFn = (<*>)

--- applicative instances

-- 1.

data Pair a
  = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair x x' = Pair (f x) (f' x')

instance Arbitrary a => Arbitrary (Pair a)  where
 arbitrary = do
   a <- arbitrary
   a' <- arbitrary
   return $ Pair a a'

instance Eq a => EqProp (Pair a) where (=-=) = eq

pairApplicativeExercise =
  quickBatch $ applicative (undefined :: Pair (Int, String, Int))

-- 2.

data Two a b
  = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  Two x f <*> Two x' y = Two (x <> x') (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)  where
 arbitrary = do
   a <- arbitrary
   b <- arbitrary
   return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

twoApplicativeExercise =
  quickBatch $ applicative (undefined :: Two [Int] (Int, String, Int))

-- 3.

data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure a = Three mempty mempty a
  Three a b f <*> Three a' b' x' = Three (a <> a') (b <> b') (f x')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)  where
 arbitrary = do
   a <- arbitrary
   b <- arbitrary
   c <- arbitrary
   return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

threeApplicativeExercise =
  quickBatch $ applicative (undefined :: Three String String (Int, String, Int))

--- 4.

data Threep a b
  = Threep a b b
  deriving (Eq, Show)

instance Functor (Threep a) where
  fmap f (Threep a b b') = Threep a (f b) (f b')

instance (Monoid a) => Applicative (Threep a) where
  pure a = Threep mempty a a
  Threep a f f' <*> Threep a' x x' = Threep (a <> a') (f x) (f' x')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Threep a b)  where
 arbitrary = do
   a <- arbitrary
   b <- arbitrary
   b' <- arbitrary
   return $ Threep a b b'

instance (Eq a, Eq b) => EqProp (Threep a b) where (=-=) = eq

threepApplicativeExercise =
  quickBatch $ applicative (undefined :: Threep String (Int, String, Int))

-- 5.

data Four a b c d
  = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure a = Four mempty mempty mempty a
  Four a b c f <*> Four a' b' c' x = Four (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
         => Arbitrary (Four a b c d)  where
 arbitrary = do
   a <- arbitrary
   b <- arbitrary
   c <- arbitrary
   d <- arbitrary
   return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

fourApplicativeExercise =
  quickBatch $ applicative (undefined :: Four String String String (Int, String, Int))

--  6.

data Fourp a b
  = Fourp a a a b
  deriving (Eq, Show)

instance Functor (Fourp a) where
  fmap f (Fourp a a' a'' b) = Fourp a a' a'' (f b)

instance (Monoid a) => Applicative (Fourp a) where
  pure a = Fourp mempty mempty mempty a
  Fourp a1 a2 a3 f <*> Fourp a1' a2' a3' x =
    Fourp (a1 <> a1') (a2 <> a2') (a3 <> a3') (f x)

instance (Arbitrary a, Arbitrary b)
         => Arbitrary (Fourp a b)  where
 arbitrary = do
   a <- arbitrary
   a' <- arbitrary
   a'' <- arbitrary
   b <- arbitrary
   return $ Fourp a a' a'' b

instance (Eq a, Eq b) => EqProp (Fourp a b) where
  (=-=) = eq

fourpApplicativeExercise =
  quickBatch $ applicative (undefined :: Fourp String (Int, String, Int))

-- combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combinations = liftA3 (,,) stops vowels stops
