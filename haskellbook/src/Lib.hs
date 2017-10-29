module Lib where

import qualified Chapter09
import qualified Chapter11

run :: IO ()
run = do
  Chapter09.test
  Chapter11.runTests
  putStrLn "Tests done!"
