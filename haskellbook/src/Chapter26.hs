{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

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

-- Reader T

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Monad m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> (ReaderT r m a) -> ReaderT r m b
  fmap f (ReaderT rma) =
    ReaderT $ (fmap . fmap) f rma


instance Monad m => Applicative (ReaderT r m)  where
  pure :: a -> ReaderT r m a
  pure = ReaderT . pure . pure

  (<*>) :: ReaderT r m (a->b) -> ReaderT r m a -> ReaderT r m b
  (<*>) = applyReaderT

applyReaderT :: Applicative m
             => ReaderT r m (a->b)
             -> ReaderT r m a
             -> ReaderT r m b
applyReaderT (ReaderT rmab) (ReaderT rma) =
  ReaderT $ (<*>) <$> rmab <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  (>>=) = bindReaderT

bindReaderT :: Monad m
            => ReaderT r m a
            -> (a -> ReaderT r m b)
            -> ReaderT r m b
bindReaderT (ReaderT rma) amb =
  ReaderT $ \r ->
      (rma r) >>=
        \a-> runReaderT (amb a) $ r

-- StateT

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }


-- 1
instance (Functor m) =>
         Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT smas) =
    let
      applyF = \(a,s) -> (f a,s)
    in
      StateT $ \s -> applyF <$> (smas s)

-- 2
instance (Monad m) =>
         Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  (<*>) :: StateT s m (a -> b)
        -> StateT s m a
        -> StateT s m b
  (<*>) = apStateT

apStateT (StateT smab) (StateT sma) =
  StateT $ \s -> do
    (ab, s') <- smab s
    (a, s'') <- sma s'
    return (ab a, s'')

-- 3
instance (Monad m) =>
         Monad (StateT s m) where
  return = pure
  (>>=) = bindStateT

bindStateT :: Monad m
           => StateT s m a
           -> (a -> StateT s m b)
           -> StateT s m b
bindStateT (StateT sma) assmb =
  StateT $ \s -> do
    (a, s') <- sma s
    runStateT (assmb a) s'

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

  describe "ReaderT" $ do
    it "functor" $ do
      let
        e :: ReaderT String [] Int
        e  = ReaderT $ const [10]
      (runReaderT e) "a" `shouldBe` [10]
    it "applicative" $ do
      let
        e :: ReaderT String [] (Int -> Int)
        e  = ReaderT $ const [(+1)]
        e' :: ReaderT String [] Int
        e'  = ReaderT $ const [10]
      (runReaderT (e <*> e')) "a" `shouldBe` [11]

    it "monad" $ do
      let
        e :: ReaderT String [] Int
        e =  ReaderT $ const [1]
        f a = return (a + 10)
      (runReaderT (e >>= f)) "a" `shouldBe` [11]
  describe "StateT" $ do
    it "functor" $ do
      let
        e :: StateT String [] Int
        e  = StateT $ const [(10, "")]
        f = (+5)
      runStateT (f <$> e) "a" `shouldBe` [(15, "")]
    it "applicative" $ do
      let
        e :: StateT String [] (Int -> Int)
        e  = StateT $ \s -> [((+5), s)]
        e'  = StateT $ \s -> [(10, s ++ " there")]
      runStateT (e <*> e') "a" `shouldBe` [(15, "a there")]
    it "monad" $ do
      let
        e :: StateT String [] Int
        e  = StateT $ \s -> [(5, s)]
        f a = StateT $ \s -> [("yo", s ++ " HOOT")]
      runStateT (e >>= f) "a" `shouldBe` [("yo", "a HOOT")]
