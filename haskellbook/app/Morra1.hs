{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad.Trans.Class
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as ST
import qualified System.Random as Rand

type ComputerScore = MkComputerSore Int
type PlayerScore   = MkPlayerScore Int

type MorraStateT a = ST.StateT MorraState IO a

type MorraGame = MorraStateT ()

data MorraState =
  MkMorraState
  { stdGen        :: Rand.StdGen
  , playerScore   :: PlayerScore
  , computerScore :: ComputerScore
  }

main :: IO ()
main = do
  stdGen <- Rand.getStdGen
  let initialState = MkMorraState stdGen (MkPlayerScore 0) (MkComputerScore 0)
  putStrLn $ show stdGen
  ST.runStateT playRound initialState
  return ()


playRound :: MorraGame
playRound = do
  state <- ST.get
  liftIO $ printScore state
  -- liftIO $ getLine
  return ()


printScore :: MorraState -> IO ()
printScore state =
  let
    pscore = playerScore state
    cscore = show $ computerScore state
    scores = "Player Score: " ++ pscore ++  " Computer Score: " ++ cscore
  in
    putStrLn scores
