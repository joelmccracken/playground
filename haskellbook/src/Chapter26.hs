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

  (<*>) = applyEitherT

applyEitherT :: Applicative m
             => EitherT e m (a -> b)
             -> EitherT e m a
             -> EitherT e m b
applyEitherT (EitherT mfab) (EitherT ma) =
  let
    x = 1
  in
    EitherT $ (<*>) <$> mfab <*> ma

-- 3

instance Monad m =>
         Monad (EitherT e m) where
  return = pure

  (>>=) = bindEitherT

bindEitherT :: Monad m
            => EitherT e m a
            -> (a -> EitherT e m b)
            -> EitherT e m b
bindEitherT (EitherT mea) faeb =
  EitherT $ do
    ea <- mea
    case ea of
      Left e -> pure (Left e)
      Right a -> runEitherT (faeb a)

-- 4

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e

swapEitherT (EitherT (meea)) =
  let
    swap :: (Either e a) -> Either a e
    swap (Left e) = Right e
    swap (Right a) = Left a
  in
    EitherT $ fmap swap meea

-- 5

eitherT :: (Functor m, Monad m)
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT amc bmc (EitherT meab) =
  (meab >>= (either amc bmc))

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

    it "applicative" $ do
      let
        e :: EitherT String [] Int
        e  = pure (5 :: Int)
        a  = pure (+7)
      runEitherT e `shouldBe` [Right 5]
      runEitherT (a <*> e) `shouldBe` [Right 12]

    it "monad" $ do
      let
        e :: EitherT String [] Int
        e  = pure (5 :: Int)
        f a = pure $ a + 11
      runEitherT (e >>= f) `shouldBe` [Right 16]

    it "swapEitherT" $ do
      let
        e :: EitherT String [] Int
        e  = pure (5 :: Int)
      runEitherT (swapEitherT e) `shouldBe` [Left 5]

    it "eitherT" $ do
      let
        e :: EitherT Int [] Int
        e  = pure (5 :: Int)
      (eitherT pure pure e) `shouldBe` [5]
