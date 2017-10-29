module Lib where

import qualified Chapter10
import qualified Chapter11

run :: IO ()
run = do
  Chapter10.test
  Chapter11.runTests
  putStrLn "Tests done!"
