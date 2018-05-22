{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter25 where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Data.Monoid
import Data.Bifunctor

newtype Identity a
  = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

newtype Compose f g a
  = Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga ) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where

  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ (fmap (<*>) f) <*> a

instance (EqProp (f (g a)), EqProp (g a), EqProp a) =>
  EqProp (Compose f g a) where
  (Compose a) =-= (Compose a') = a =-= a'

instance (Arbitrary (f (g a)), Arbitrary (g a), Arbitrary a) =>
         Arbitrary (Compose f g a) where
  arbitrary = Compose <$> arbitrary

qbComposeFunctor :: IO ()
qbComposeFunctor = do
  putStrLn "app compose"
  let trigger :: Compose [] Maybe (Int, Int, Int)
      trigger = undefined
  quickBatch (functor trigger)

qbComposeApplicative :: IO ()
qbComposeApplicative = do
  putStrLn "app compose"
  let trigger :: Compose [] Maybe (Int, Int, Int)
      trigger = undefined
  quickBatch (applicative trigger)

instance (Foldable f, Foldable g) =>
         Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = (foldMap $ foldMap f) fga


instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
  traverse = traverseCompose

traverseCompose :: (Traversable f, Traversable g, Applicative t)
                => (a -> t b)
                -> Compose f g a
                -> t (Compose f g b)
traverseCompose atb (Compose fga) =
  let
    ftgb = (traverse atb) <$> fga
    tfgb = (traverse id) ftgb
  in
    fmap Compose tfgb

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance EqProp a => EqProp (Sum a) where
  (Sum a) =-= (Sum a') = a =-= a'

qbComposeTraversable :: IO ()
qbComposeTraversable = do
  putStrLn "traversable compose"
  let trigger :: Compose [] Maybe (Int, Int, Sum Int)
      trigger = undefined
  quickBatch (traversable trigger)

-- bifunctor implementations

data Deux a b
  = Deux a b
  deriving (Eq, Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b
  = Const a
  deriving (Eq, Show)

instance Bifunctor Const where
  bimap f g (Const a) = Const (f a)

data Drei a b c
  = Drei a b c
  deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c
  = SuperDrei a b
  deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c
  = SemiDrei a
  deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei a) = SemiDrei a

data Quadriceps a b c d
  = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b
  = Left' a
  | Right' b

instance Bifunctor Either' where
  bimap f g (Left' a) = Left' (f a)
  bimap f g (Right' b) = Right' (g b)

newtype MaybeIO a =
  MaybeIO { runMaybeIO :: IO (Maybe a) }

newtype MaybeLit a =
  MaybeList { runMaybeList :: [Maybe a] }

------ IdentityT

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) =
    Identity (f a)

newtype IdentityT f a
  = IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor m) =>
         Functor (IdentityT m) where
  fmap f (IdentityT fa) =
    IdentityT (fmap f fa)

instance (Applicative m) =>
         Applicative (IdentityT m ) where
  pure = IdentityT . pure

  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure

  (Identity a) >>= f = f a


instance (Monad m) =>
         Monad (IdentityT m) where
  return = pure

  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f
