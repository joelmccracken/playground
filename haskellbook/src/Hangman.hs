module Hangman where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
      in
        l >= minWordLength &&
        l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO ( minWordLength, maxWordLength)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle str =
  Puzzle str (fmap (const Nothing) str) []


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle solution _ _) guess =
  elem guess solution


alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ guesses incorrectGuesses) guess =
  elem guess incorrectGuesses ||
  elem (Just guess) guesses

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar char =
  case char of
    Just c -> c
    Nothing -> '_'

addIncorrectGuess :: Puzzle -> Char -> Puzzle
addIncorrectGuess (Puzzle word filledInSoFar s) char  =
  (Puzzle word filledInSoFar (char : s))

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word
                 filledInSoFar s) c =
  Puzzle word newFilledInSoFar s
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar

    newFilledInSoFar =
      zipWith (zipper c)
        word filledInSoFar

data GuessResult
  = AlreadyGuessed Puzzle
  | WasInWord Puzzle
  | WasNotInWord Puzzle


handleGuess' :: Puzzle -> Char -> GuessResult
handleGuess' puzzle guess = do
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
    (True, True) -> do
      AlreadyGuessed puzzle

    (True, False) -> do
      WasInWord (fillInCharacter puzzle guess)

    (False, True) -> do
      AlreadyGuessed puzzle

    (False, False) -> do
      WasNotInWord (addIncorrectGuess puzzle guess)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case handleGuess' puzzle guess of
    AlreadyGuessed puzzle -> do
      putStrLn "You already guessed that\
               \ character, pick \
               \ something else!"
      return puzzle

    WasInWord puzzle -> do
      putStrLn "This character was in the\
               \ word, filling in the word\
               \ accordingly"
      return puzzle

    WasNotInWord puzzle -> do
      putStrLn "This character wasn't in\
               \ the word, try again."
      return puzzle


maxGuesses = 15

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > maxGuesses then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ ->
      putStrLn "Your guess must\
               \ be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle
