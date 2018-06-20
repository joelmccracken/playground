{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad.Trans.State as ST
import qualified System.Random as Rand
type ComputerScore = Int
type PlayerScore = Int

type Morra a = ST.StateT (PlayerScore, ComputerScore) IO a

main = main1

-- pvc
main1 :: IO ()
main1 = do
  initial <- (Rand.randomRIO (0,1))  :: IO Int
  putStrLn $ show initial

  return ()
