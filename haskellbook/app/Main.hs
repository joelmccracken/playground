module Main where

import System.Environment (getArgs, withArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)

import qualified Chapter09
import qualified Chapter10
import qualified Chapter11
import qualified Chapter12
import qualified Chapter13
import qualified Hangman
import qualified Chapter14
import qualified Morse
import qualified Chapter15
import qualified Chapter16
import qualified Chapter17



main :: IO ()
main = do
  mode <- getArgs
  case mode of
    (arg:args) ->
      case arg of
        "earlier" -> putStrLn "FIXME"
        "ch13-hangman" -> Hangman.main
        "ch13-cipher" -> Chapter13.runCipher
        "ch13-palindrome" -> Chapter13.runPalindrome
        "ch13-palindrome-better" -> Chapter13.runPalindromeBetter
        "ch13-gimmie-person" -> Chapter13.gimmePerson
        "ch14" -> withArgs [] Chapter14.main
        "ch14-morse" ->
          case args of
            -- remove initial ch14-morse from args
            [arg] -> withArgs [arg] $ Morse.main
            _      -> argError
        "ch14-morse-test" ->
          Morse.morseTest
        "ch15" -> withArgs [] $ Chapter15.main
        "ch16" -> withArgs [] $ Chapter16.main
        "ch17" -> withArgs [] $ Chapter17.main
    _ -> argError
  where
    argError = do
      putStrLn "check source for usage"
      exitFailure
