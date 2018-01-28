module Chapter13 where

import qualified Chapter09 as C09
import qualified Chapter11 as C11

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


run = do
  putStrLn "Running Caesar"
  c <- caesarIO
  putStrLn $ "done Running Caesar: " ++ c
  putStrLn "Running vigenere"
  v <- vigenereIO
  putStrLn $ "done Running vigenere: " ++ v
