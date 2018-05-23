{-# LANGUAGE FlexibleContexts #-}

module Chapter26 where

-- import Test.QuickCheck.Checkers
-- import Test.QuickCheck.Classes

-- import Data.Either
import Test.Hspec


newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) =>
         Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance (Applicative m) =>
         Applicative (MaybeT m) where
  pure x = MaybeT $ pure (pure x)

  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> fab <*> mma


instance (Monad m) =>
         Monad (MaybeT m) where
  return = pure

  (>>=) = bindMaybeT


bindMaybeT :: Monad m
           => MaybeT m a
           -> (a -> MaybeT m b)
           -> MaybeT m b
bindMaybeT (MaybeT ma) fammb =
  let
    maybeConvert =
      maybe
        (return Nothing)
        (runMaybeT . fammb)
  in MaybeT $ ma >>= maybeConvert


-- EitherT

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Show (EitherT e m a) where
  show _ = "EitherT"

-- 1

instance Functor m =>
         Functor (EitherT e m) where
  fmap = eitherTFmap

eitherTFmap :: Functor m
            => (a -> b)
            -> EitherT e m a
            -> EitherT e m b
eitherTFmap fab (EitherT meea) =
  EitherT $ (fmap . fmap) fab meea

-- 2

instance Applicative m =>
         Applicative (EitherT e m) where
  pure = EitherT . pure . Right

  f <*> a = undefined


main = hspec $ do
  describe "EitherT" $ do
    it "functor" $ do
      let
        e :: EitherT String [] Int
        e  = (+1) <$> (EitherT [Left "error"])
      let
        e' :: EitherT String [] Int
        e' = (+1) <$> (EitherT [Right 2])
      runEitherT e `shouldBe` [Left "error"]
      runEitherT e' `shouldBe` [Right 3]
