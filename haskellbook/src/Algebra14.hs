module Algebra14 where

data Quantum
  = Yes
  | No
  | Both

convert1 :: Quantum -> Bool
convert1 Yes  = False
convert1 No   = False
convert1 Both = False

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = False
convert2 Both = False


convert3 :: Quantum -> Bool
convert3 Yes  = False
convert3 No   = True
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes  = True
convert4 No   = True
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes  = False
convert5 No   = False
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes  = True
convert6 No   = False
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = True
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes  = True
convert8 No   = True
convert8 Both = True

{-
exercises quad p 672
1. 4 + 4
2. 4 * 4
3. 4 ^ 4
4. 4 * 4 * 4
5. 2 ^ (2 * 2)
6. 4 ^ (4 * 2) = (2^2)^(4*2) = 2^(2*4*2) = 2^16 = 65536
-}
