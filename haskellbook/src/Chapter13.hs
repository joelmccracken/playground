module Chapter13 where

import qualified Chapter09 as C09
import qualified Chapter11 as C11

import System.Exit (exitSuccess)
import Control.Monad
import Data.Char
-- check for understanding
-- 1. forever, when
-- 2. unqualified: mask, try, forever, when, all of data.bits, fromlistbe, chunksof, database.blacktip.types, unsafeperformio. Entirety: Data.bits, Database.blacktip.types
-- 3. the variety of types used internally for the Blacktype library
-- 4. a. Control.Concurrent.MVar, Filesystem.Path.CurrentOS, Control.Concurrent
--    b. import qualified Filesystem as FS
--    c. import Control.Monad (forever, when)
--

-- modifying code
-- 1.
caesarIO :: IO String
caesarIO = do
  putStrLn "number: "
  snum <- getLine
  let num = read snum :: Int
  putStrLn "message: "
  msg <- getLine
  return (C09.caesar num msg)

vigenereIO :: IO String
vigenereIO = do
  putStrLn "cipher: "
  cipher <- getLine
  putStrLn "message: "
  msg <- getLine
  return (C11.vigenere cipher msg)


runCipher = do
  putStrLn "Running Caesar"
  c <- caesarIO
  putStrLn $ "done Running Caesar: " ++ c
  putStrLn "Running vigenere"
  v <- vigenereIO
  putStrLn $ "done Running vigenere: " ++ v

-- 2

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!"
                exitSuccess

runPalindrome = do
  putStrLn "palindrome test:"
  palindrome

-- 3

palindromeBetter :: IO ()
palindromeBetter = forever $ do
  line1 <- getLine
  let normalized = filter isAlpha $ map toLower line1
  case (normalized == reverse normalized) of
    True -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!"
                exitSuccess

runPalindromeBetter = do
  putStrLn "palindrome test:"
  palindromeBetter


-- 4

type Name = String
type Age = Integer
data Person = Person Name Age
  deriving Show

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)
mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown
    $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "person name: "
  name <- getLine
  putStrLn "person age: "
  sAge <- getLine
  let age = read sAge :: Integer
  case (mkPerson name age) of
    Left error -> do
      putStrLn $ "an error occurred: " ++ show error

    Right person -> do
      putStrLn $ "Yay! Successfully got a person: " ++ show person
