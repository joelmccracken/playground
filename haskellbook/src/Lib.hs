module Lib where

import qualified Chapter09
import qualified Chapter10
import qualified Chapter11
import qualified Chapter12
import qualified Chapter13
import qualified Chapter14

run :: IO ()
run = do
  -- Chapter09.test
  -- Chapter10.test
  -- Chapter11.runTests
  -- Chapter12.runTests
  -- Chapter13.run
  Chapter14.main
  putStrLn "done!"
