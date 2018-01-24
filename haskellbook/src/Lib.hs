module Lib where

import qualified Chapter09
import qualified Chapter10
import qualified Chapter11
import qualified Chapter12

run :: IO ()
run = do
  Chapter09.test
  Chapter10.test
  Chapter11.runTests
  Chapter12.runTests
  putStrLn "Tests done!"
