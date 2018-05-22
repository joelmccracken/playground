module Chapter26 where

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
