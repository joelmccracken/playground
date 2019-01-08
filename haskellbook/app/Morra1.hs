{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad.Trans.Class
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as ST
import qualified System.Random as Rand
import Text.Read (readMaybe)
import Control.Monad (guard)
import System.Exit (exitSuccess)

newtype ComputerScore
  = MkComputerScore Int
  deriving (Show, Eq)

newtype PlayerScore
  = MkPlayerScore Int
  deriving (Show, Eq)

type MorraGame a =
  ST.StateT MorraState IO a

data MorraState =
  MkMorraState
  { stdGen        :: Rand.StdGen
  , playerScore   :: PlayerScore
  , computerScore :: ComputerScore
  }

incPlayerWins :: MorraGame ()
incPlayerWins = do
  state <- ST.get
  let (MkPlayerScore score) = playerScore state
  let newState = state { playerScore = MkPlayerScore (score  + 1) }
  ST.put newState

incComputerWins :: MorraGame ()
incComputerWins = do
  state <- ST.get
  let (MkComputerScore score) = computerScore state
  let newState = state { computerScore = MkComputerScore (score  + 1) }
  ST.put newState

main :: IO ()
main = do
  stdGen <- Rand.getStdGen
  let initialState = MkMorraState stdGen (MkPlayerScore 0) (MkComputerScore 0)
  putStrLn $ show stdGen
  ST.runStateT playRound initialState
  return ()

getRandom :: Int -> Int -> MorraGame Int
getRandom lo hi = do
  state <- ST.get
  let rng = stdGen state
  let (newRandom, rng') = Rand.randomR (lo, hi) rng
  ST.put state {stdGen = rng'}
  return newRandom

readNum :: Int -> Int -> String -> Maybe Int
readNum min max str =
  let
    r :: Maybe Int
    r = readMaybe str

    bounded :: Int -> Maybe Int
    bounded i =
      if min <= i && max >= i then
        Just i
      else
        Nothing
  in
    r >>= bounded

playRound :: MorraGame ()
playRound = do
  state <- ST.get
  liftIO $ printScore state

  wonCheck

  playerNum <- liftIO $ do
    putStrLn "Number of fingers to show (0-5): "
    readNum 0 5 <$> getLine

  case playerNum of
    Just num ->
      playWithPlayerChoice num
    Nothing -> do
      liftIO $ putStrLn "Invalid number of fingers to show."
      playRound

playWithPlayerChoice :: Int -> MorraGame ()
playWithPlayerChoice playerChoice = do
  guess <- liftIO $ do
    putStrLn "Guess at total number of fingers shown: (0-10): "
    readNum 0 10 <$> getLine
  case guess of
    Just num ->
      playWithPlayerChoiceAndGuess playerChoice num
    Nothing -> do
      liftIO $ putStrLn "Invalid guess."
      playRound

playWithPlayerChoiceAndGuess :: Int -> Int -> MorraGame ()
playWithPlayerChoiceAndGuess playerChoice playerGuess = do
  computerChoice <- getRandom 0 5
  computerGuess <- getRandom 0 10

  let totalShown = playerChoice + computerChoice
  liftIO $ putStrLn $ "Computer shown: " ++ show computerChoice
  liftIO $ putStrLn $ "Computer guess: " ++ show computerGuess
  liftIO $ putStrLn $ "Actual total shown: " ++ show totalShown

  case (totalShown == playerGuess, totalShown == computerGuess) of
    (True, True) -> do
      liftIO $ putStrLn "Tie!"
    (True, False) -> do
      liftIO $ putStrLn "You won that round!"
      incPlayerWins
    (False, True) -> do
      liftIO $ putStrLn "Computer won the round."
      incComputerWins
    _ ->
      liftIO $ putStrLn "No winner."

  playRound
  return ()

printScore :: MorraState -> IO ()
printScore state =
  let
    MkPlayerScore pscore = playerScore state
    MkComputerScore cscore = computerScore state
    scores = "Player Score: " ++ show pscore ++  " Computer Score: " ++ show cscore
  in
    putStrLn scores


wonCheck :: MorraGame ()
wonCheck = do
  state <- ST.get
  let
    MkPlayerScore pscore = playerScore state
    MkComputerScore cscore = computerScore state
  if pscore > 2 then do
    liftIO $ putStrLn "You win!"
    liftIO $ exitSuccess
  else if cscore > 2 then do
    liftIO $ putStrLn "You lose!"
    liftIO $ exitSuccess
  else
    return ()
