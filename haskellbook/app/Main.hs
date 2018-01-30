module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)

import qualified Lib

import qualified Chapter09
import qualified Chapter10
import qualified Chapter11
import qualified Chapter12
import qualified Chapter13
import qualified Chapter14


main :: IO ()
main = do
  mode <- getArgs
  putStrLn $ show mode
  case mode of
    [arg] ->
      case arg of
        "earlier" -> putStrLn "FIXME"
        "ch13" -> return ()
        "ch13-cipher" -> Chapter13.runCipher
        "ch13-palindrome" -> Chapter13.runPalindrome
        "ch13-palindrome-better" -> Chapter13.runPalindromeBetter
        "ch13-gimmie-person" -> Chapter13.gimmePerson
        "ch14" -> Chapter14.main
        _      -> argError
    _ -> argError
  where
    argError = do
      putStrLn "check source for usage"
      exitFailure
