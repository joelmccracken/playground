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
import qualified Chapter18
import qualified Chapter20
import qualified Chapter21
import qualified Chapter22

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
        "ch17-list-app" -> withArgs [] $ Chapter17.listApplicativeExercise
        "ch17-ziplist-app" -> withArgs [] $ Chapter17.zipListApplicativeExercise
        "ch17-pair-app" -> withArgs [] $ Chapter17.pairApplicativeExercise
        "ch17-two-app" -> withArgs [] $ Chapter17.twoApplicativeExercise
        "ch17-three-app" -> withArgs [] $ Chapter17.threeApplicativeExercise
        "ch17-threep-app" -> withArgs [] $ Chapter17.threepApplicativeExercise
        "ch17-four-app" -> withArgs [] $ Chapter17.fourApplicativeExercise
        "ch17-fourp-app" -> withArgs [] $ Chapter17.fourpApplicativeExercise
        "ch18-either-monad" -> withArgs [] $ Chapter18.eitherMonadExercise
        "ch18-nope-monad" -> withArgs [] $ Chapter18.nopeMonadExercise
        "ch18-phbteither-monad" -> withArgs [] $ Chapter18.phbtEitherMonadExercise
        "ch18-identity-monad" -> withArgs [] $ Chapter18.identityMonadExercise
        "ch18-list-monad" -> withArgs [] $ Chapter18.listMonadExercise
        "ch18-implement" -> withArgs [] $ Chapter18.implementExercises
        "ch20" -> withArgs [] $ Chapter20.main
        "ch21-identity" -> withArgs [] $ Chapter21.qbIdentity
        "ch21-constant" -> withArgs [] $ Chapter21.qbConstant
        "ch21-optional" -> withArgs [] $ Chapter21.qbOptional
        "ch21-list" -> withArgs [] $ Chapter21.qbList
        "ch21-three" -> withArgs [] $ Chapter21.qbThree
        "ch21-pair" -> withArgs [] $ Chapter21.qbPair
        "ch21-big" -> withArgs [] $ Chapter21.qbBig
        "ch21-bigger" -> withArgs [] $ Chapter21.qbBigger
        "ch21-s" -> withArgs [] $ Chapter21.qbS
        "ch21-tree" -> withArgs [] $ Chapter21.qbTree
        "ch22" -> withArgs [] $ Chapter22.main
        _ -> argError
    _ -> argError
  where
    argError = do
      putStrLn "check source for usage"
      exitFailure
