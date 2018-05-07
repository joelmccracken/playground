{-# LANGUAGE FlexibleContexts #-}

module Chapter21 where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (traversable)

-- import Data.Monoid
-- import Data.Semigroup
-- import Data.Foldable
-- import Data.Bool

class (Functor t, Foldable t)
  => Traversable' t where
  traverse' :: Applicative f
           => (a -> f b)
           -> t a
           -> f (t b)
  traverse' f = sequenceA' . fmap f

  sequenceA' :: Applicative f
             => t (f a)
             -> f (t a)
  sequenceA' = traverse' id


data TEither a b
  = TLeft a
  | TRight b
  deriving (Eq, Ord, Show)

instance Functor (TEither a) where
  fmap _ (TLeft x) = TLeft x
  fmap f (TRight y) = TRight (f y)

instance Applicative (TEither e) where
  pure = TRight
  TLeft e <*> _ = TLeft e
  TRight f <*> r = fmap f r

instance Foldable (TEither a) where
  foldMap _ (TLeft _) = mempty
  foldMap f (TRight y) = f y

  foldr _ z (TLeft _) = z
  foldr f z (TRight y) = f y z

instance Traversable (TEither a) where
  traverse _ (TLeft x) = pure (TLeft x)
  traverse f (TRight y) = TRight <$> f y

--------------------------------------------------------------------------------

newtype Identity a
  = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = (f a)

instance Traversable Identity where
  traverse = traverseIdentity

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

instance EqProp a
    => EqProp (Identity a) where
  (=-=) = identityEq

identityEq :: EqProp a
           => Identity a
           -> Identity a
           -> Property
(Identity a) `identityEq` (Identity b) =
  a =-= b

traverseIdentity :: Functor f
                 => (a -> f b)
                 -> Identity a
                 -> f (Identity b)
traverseIdentity f (Identity a) = Identity <$> (f a)

qbIdentity :: IO ()
qbIdentity = do
  putStrLn "identity"
  let trigger :: Identity (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-----------------------------------------------------------------------------------------------

newtype Constant a b
  = Constant { getConstant :: a }
  deriving (Show, Eq)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap f (Constant x) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

instance EqProp a => EqProp (Constant a b) where
  (Constant a) =-= (Constant b) = a =-= b

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = arbitrary >>= pure . Constant

qbConstant :: IO ()
qbConstant = do
  putStrLn "constant"
  let trigger :: Constant Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

--------------------------------------------------------------------------------

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
  foldMap f (Yep a) = (f a)
  foldMap f Nada    = mempty

instance Functor Optional where
  fmap f (Yep a) = Yep (f a)
  fmap f _       = Nada

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, pure Nada), (10, Yep <$> arbitrary) ]

instance Traversable Optional where
  traverse = traverseOptional

traverseOptional :: Applicative f
                 => (a -> f b)
                 -> Optional a
                 -> f (Optional b)
traverseOptional g opt =
  case opt of
    (Yep a) -> Yep <$> (g a)
    Nada -> pure Nada

qbOptional :: IO ()
qbOptional = do
  putStrLn "optional"
  let trigger :: Optional (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)


--------------------------------------------------------------------------------

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap = mapList

mapList :: (a -> b)
        -> List a
        -> List b
mapList f ls =
  case ls of
    Nil -> Nil
    Cons a ls' -> Cons (f a) $ mapList f ls'

instance Foldable List where
  foldr = foldrList

foldrList :: (a -> b -> b)
          -> b
          -> (List a)
          -> b
foldrList f initial ls =
  case ls of
    Nil -> initial
    (Cons x ls')  -> f x (foldrList f initial ls')

instance Traversable List where
  sequenceA = sequenceAList

sequenceAList :: Applicative f
              => List (f a)
              -> f (List a)
sequenceAList ls =
  case ls of
    Nil -> pure Nil
    Cons fa fas ->
      (Cons <$> fa) <*> sequenceAList fas

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    frequency [(1, return Nil),
               (10, Cons <$> arbitrary <*> arbitrary )]

qbList :: IO ()
qbList = do
  putStrLn "list"
  let trigger :: List (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)


--------------------------------------------------------------------------------
data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
         => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c)
    => EqProp (Three a b c) where
  (=-=) = eq

instance Traversable (Three a b) where
  traverse = traverseThree

traverseThree :: Functor f
                 => (c -> f d)
                 -> Three a b c
                 -> f (Three a b d)
traverseThree f (Three a b c) = Three a b <$> (f c)

qbThree :: IO ()
qbThree = do
  putStrLn "three"
  let trigger :: Three Int Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

--------------------------------------------------------------------------------

data Pair a b
  = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = (f b)

instance (Arbitrary a, Arbitrary b)
         => Arbitrary (Pair a b) where
  arbitrary = do

    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance (Eq a, Eq b)
    => EqProp (Pair a b) where
  (=-=) = eq

instance Traversable (Pair a) where
  traverse = traversePair

traversePair :: Functor f
                 => (b -> f c)
                 -> Pair a b
                 -> f (Pair a c)
traversePair f (Pair a b) = Pair a <$> (f b)

qbPair :: IO ()
qbPair = do
  putStrLn "pair"
  let trigger :: Pair Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-------------------------------------------------------

data Big a b
  = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap = foldMapBig

foldMapBig :: Monoid m
           => (b -> m)
           -> (Big a b)
           -> m

foldMapBig f (Big a b b') = (f b) `mappend` (f b')

instance (Arbitrary a, Arbitrary b)
         => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Big a b b'

instance (Eq a, Eq b)
    => EqProp (Big a b) where
  (=-=) = eq

instance Traversable (Big a) where
  traverse = traverseBig

traverseBig :: Applicative f
            => (b -> f c)
            -> Big a b
            -> f (Big a c)
traverseBig f (Big a b b') = Big a <$> (f b) <*> (f b')

qbBig :: IO ()
qbBig = do
  putStrLn "big"
  let trigger :: Big Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-------------------------------------------------------

data Bigger a b
  = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap = foldMapBigger

foldMapBigger :: Monoid m
           => (b -> m)
           -> (Bigger a b)
           -> m
foldMapBigger f (Bigger a b b' b'') = (f b) `mappend` (f b') `mappend` (f b'')

instance (Arbitrary a, Arbitrary b)
         => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    b'' <- arbitrary
    return $ Bigger a b b' b''

instance (Eq a, Eq b)
    => EqProp (Bigger a b) where
  (=-=) = eq

instance Traversable (Bigger a) where
  traverse = traverseBigger

traverseBigger :: Applicative f
            => (b -> f c)
            -> Bigger a b
            -> f (Bigger a c)
traverseBigger f (Bigger a b b' b'') = Bigger a <$> (f b) <*> (f b') <*> (f b'')

qbBigger :: IO ()
qbBigger = do
  putStrLn "bigger"
  let trigger :: Bigger Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

--------------------------------------------------------------------------------

data S n a
  = S (n a) a
  deriving (Eq, Show)

instance Functor n
      => Functor (S n) where
  fmap f (S na a) =
    S (f <$> na) (f a)

instance Foldable n
      => Foldable (S n) where
  foldMap g (S na a) = (foldMap g na) `mappend` (g a)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
         => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary


-- instance given in book is wrong;
-- correct one given by christoph horst (Thanks!)
-- instance ( Applicative n
--          , Testable (n Property)
--          , EqProp a )
--         => EqProp (S n a) where
--   (S x y) =-= (S p q) =
--         (property $ (=-=) <$> x <*> p)
--     .&. (y =-= q)

instance (EqProp a, EqProp (n a)) => EqProp (S n a) where
   (S x y) =-= (S p q) = (x =-= p) .&. (y =-= q)

instance Traversable n
      => Traversable (S n) where
  traverse = traverseS

traverseS :: (Traversable n, Applicative f)
          => (b -> f c)
          -> S n b
          -> f (S n c)
traverseS g (S na a) =
  let
    flippedNofA = traverse g na
    flippedA = g a
  in
    S <$> flippedNofA <*> flippedA

qbS :: IO ()
qbS = do
  putStrLn "S"
  let trigger :: S [] (Int, Int, [Int])
      trigger = undefined
  verboseBatch (traversable trigger)
