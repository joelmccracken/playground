module Lib where

import qualified Chapter11

run :: IO ()
run = do
  Chapter11.runTests
  putStrLn "Tests done!"
