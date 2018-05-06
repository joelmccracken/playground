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
