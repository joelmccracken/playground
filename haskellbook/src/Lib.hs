module Lib where

import qualified Algebra14
import qualified BinaryTree11_14

run :: IO ()
run = do
  BinaryTree11_14.testMapTree
  BinaryTree11_14.testPreorder
  BinaryTree11_14.testInorder
  BinaryTree11_14.testPostorder
  BinaryTree11_14.testFoldTree
  putStrLn "Tests done!"
