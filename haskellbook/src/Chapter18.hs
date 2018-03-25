module Chapter18 where

import Data.Monoid ((<>))
import Test.Hspec
import Test.QuickCheck (Arbitrary, arbitrary, frequency, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA2)



twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

-- short exercise: either monad

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

instance Monoid a => Applicative (Sum a) where
  pure x = Second x

  (Second f) <*> (Second x) = Second (f x)
  (First a) <*> (First b) = First (a <> b)
  (First a) <*> _ = First a
  _ <*> (First b) = First b


instance Monoid a => Monad (Sum a) where
  return = pure
  Second x >>= f = f x
  First a >>= _ = First a

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b)  where
 arbitrary = do
   a <- arbitrary
   b <- arbitrary
   elements [First a, Second b]

eitherMonadExercise =
  quickBatch $ monad (undefined :: Sum String (Int, String, Int))

-- chapter exercies

-- 1. monad nope

data Nope a
  = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return a = NopeDotJpg
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

nopeMonadExercise =
  quickBatch $ monad (undefined :: Nope (Int, String, Int))

-- 2. PhbtEither

data PhbtEither b a
  = PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (PhbtEither b) where
  fmap f (PLeft a) = PLeft (f a)
  fmap _ (PRight b) = PRight b

instance Applicative (PhbtEither b) where
  pure a = PLeft a

  PLeft f <*> PLeft x = PLeft $ f x
  PRight b <*> _ = PRight b
  _ <*> PRight b = PRight b

instance Monad (PhbtEither b) where
  return a = PLeft a
  PLeft a >>= f = f a
  PRight b >>= _ = PRight b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [ PLeft b, PRight a ]

instance (Eq a, Eq b) => EqProp (PhbtEither b a) where
  (=-=) = eq

phbtEitherMonadExercise =
  quickBatch $ monad (undefined :: PhbtEither Int (Int, String, Int))


-- 3. Identity monad

newtype Identity a
  = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

identityMonadExercise =
  quickBatch $ monad (undefined :: Identity (Int, String, Int))


-- 4. list monad

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

listBind :: List a -> (a -> List b) -> List b
listBind (Cons a as) f = listConcat (f a) (listBind as f)
listBind Nil _ = Nil

listConcat :: List a -> List a -> List a
listConcat Nil ls = ls
listConcat (Cons x xs) ls = (Cons x (listConcat xs ls))

instance Monad List where
  return x = Cons x Nil
  (>>=) = listBind

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    ls <- arbitrary
    frequency [ (1, return $ Nil)
              , (10, return $ Cons a ls)
              ]

instance Eq a => EqProp (List a) where (=-=) = eq

listMonadExercise =
  quickBatch $ monad (undefined :: List (Int, String, Int))


--- implement function exercises

-- 1.

j :: Monad m => m (m a) -> m a
j = (>>= id)

-- 2.

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

-- 4.

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh as f =
  meh' as [] f

meh' :: Monad m => [a] -> [b] -> (a -> m b) -> m [b]
meh' [] bs _ = return $ reverse bs
meh' (a:as) bs f = do
  b <- (f a)
  meh' as (b:bs) f

-- 6. flipType

flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id

implementExercises = hspec $ do
  it "1. j" $ do
    j [[1,2], [], [3]] `shouldBe` [1,2,3]
    j (Just (Just 1)) `shouldBe` (Just 1)
    j (Just Nothing) `shouldBe` (Nothing :: Maybe Int)
    j Nothing `shouldBe` (Nothing :: Maybe Int)
  it "5. meh" $ do
    -- added a test because this was weird enough i wanted to see it work
    meh [1..2] (\x-> [(x-1,x)]) `shouldBe` [[(0,1), (1,2)]]

  it "6. flipType" $ do
    flipType [[1..2]] `shouldBe` [[1], [2]]
