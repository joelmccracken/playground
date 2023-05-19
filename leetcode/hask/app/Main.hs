{-# LANGUAGE BangPatterns #-}

module Main where

main :: IO ()
main = putStrLn $ show $ fib 100

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib !n = fibh 2 1 1
  where
    fibh !i !m_1 !m_2 =
      let
        thisVal = m_1 + m_2
      in
        if i == n then
          thisVal
        else
          fibh (i + 1) thisVal m_1
