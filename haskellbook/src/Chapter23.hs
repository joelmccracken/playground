{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Chapter23 where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    _ -> error $ "can only make die from ints 1-6, got: " ++ show n

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1,6) s
      (d2, s2) = randomR (1,6) s1
      (d3, _) = randomR (1,6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1,6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1,6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie


rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = rollsToGetN 20

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= limit = count
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit g = go 0 (0, []) g
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum (count, prev) gen
      | sum >= limit = (count, prev)
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die) (count + 1, (intToDie die) : prev ) nextGen

newtype Moi s a
  = Moi { runMoi :: s -> (a, s) }

instance Show (Moi s a) where
  show _ = "Moi[s,a]"

instance (Arbitrary a, Arbitrary b, CoArbitrary a) => Arbitrary (Moi a b) where
  arbitrary = Moi <$> arbitrary

instance (Arbitrary a, Show a, EqProp a, EqProp b) => EqProp (Moi a b) where
  (Moi a) =-= (Moi a') = a =-= a'
instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap = moiFmap

moiFmap f (Moi g) =
  Moi $ \x-> (f $ fst (g x), snd (g x))

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a,s)

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (<*>) = moiAp


moiAp (Moi f) (Moi g) = Moi go
  where
    go s =
      let
        (a, sg)   = g s
        (aToB,sf) = f sg
          -- should it be this state? or other?
      in
        (aToB a, sf)

instance Monad (Moi a) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (>>=) = moiBind

moiBind (Moi f) g = Moi go
  where
    go s =
      let
        (a, s') = f s
        msb = (g a)
      in
        (runMoi msb) s'

qbMoiFunctor :: IO ()
qbMoiFunctor = do
  putStrLn "app moi"
  let trigger :: Moi Int (Int, Int, Int)
      trigger = undefined
  quickBatch (functor trigger)

qbMoiApplicative :: IO ()
qbMoiApplicative = do
  putStrLn "app moi"
  let trigger :: Moi Int (Int, Int, Int)
      trigger = undefined
  quickBatch (applicative trigger)

qbMoiMonad :: IO ()
qbMoiMonad = do
  putStrLn "monad moi"
  let trigger :: Moi Int (Int, Int, Int)
      trigger = undefined
  quickBatch (monad trigger)




main = hspec $ do
  it "functor" $ do
   runMoi ((+1) <$> (Moi $ (0,))) 0 `shouldBe` (1,0)
