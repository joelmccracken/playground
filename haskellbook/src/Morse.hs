module Morse (main, morseTest) where

import qualified Data.Map as M
import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)

import Test.QuickCheck

type Morse = String

letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList [
    ('a', ".-")
  , ('b', "-...")
  , ('c', "-.-.")
  , ('d', "-..")
  , ('e', ".")
  , ('f', "..-.")
  , ('g', "--.")
  , ('h', "....")
  , ('i', "..")
  , ('j', ".---")
  , ('k', "-.-")
  , ('l', ".-..")
  , ('m', "--")
  , ('n', "-.")
  , ('o', "---")
  , ('p', ".--.")
  , ('q', "--.-")
  , ('r', ".-.")
  , ('s', "...")
  , ('t', "-")
  , ('u', "..-")
  , ('v', "...-")
  , ('w', ".--")
  , ('x', "-..-")
  , ('y', "-.--")
  , ('z', "--..")
  , ('1', ".----")
  , ('2', "..---")
  , ('3', "...--")
  , ('4', "....-")
  , ('5', ".....")
  , ('6', "-....")
  , ('7', "--...")
  , ('8', "---..")
  , ('9', "----.")
  , ('0', "-----")
  ]

morseToLetter :: M.Map Morse Char
morseToLetter =
  M.foldWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c =
  M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse s =
  sequence $ fmap charToMorse s

morseToChar :: Morse -> Maybe Char
morseToChar m =
  M.lookup m morseToLetter


convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess

  line <- hGetLine stdin
  convertLine line

  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just str)
          -> putStrLn
             (intercalate " " str)
        Nothing
          -> do
            putStrLn $ "ERROR: " ++ line
            exitFailure


convertFromMorse :: IO ()
convertFromMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess

  -- otherwise, proceed
  line <- hGetLine stdin
  convertLine line

  where
    convertLine line = do
      let decoded :: Maybe String
          decoded =
            traverse morseToChar
                     (words line)
      case decoded of
        (Just s) -> putStrLn s
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure


prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen (\c -> ((charToMorse c) >>= morseToChar) == Just c)


allowedChars :: [Char]
allowedChars = M.keys letterToMorse
allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

morseTest = quickCheck prop_thereAndBackAgain

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to"   -> convertToMorse
        _      -> argError
    _ -> argError

  where argError = do
          putStrLn "Please specifcy the \
                   \ argument \
                   \ as being 'from' or\
                   \ 'to'"

          exitFailure
