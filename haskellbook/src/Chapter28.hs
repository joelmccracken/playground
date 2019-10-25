module Chapter28 where

import Criterion.Main
import Test.Hspec
import Data.Maybe (fromMaybe)

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL (x:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList x = (unDL x) []
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

dlistTests = do
  describe "DList" $ do
    it "empty and toList" $ do
      (toList $ empty) `shouldBe` ([] :: [Int])

    let dl21 = cons 2 $ cons 1 empty
    it "describing cons" $ do
      (toList $ dl21) `shouldBe` [2, 1]

    let dl217 = snoc dl21 7
    it "snoc" $ do
      (toList $ dl217) `shouldBe` [2, 1, 7]

    let dl5 = singleton 5
    it "singleton" $ do
      (toList $ dl5) `shouldBe` [5]

    let dl21217 = append dl21 dl217
    it "append" $ do
      (toList $ dl21217) `shouldBe` [2,1,2,1,7]

main' :: IO ()
main' = defaultMain
  [ bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456
  ]

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

emptyQ = Queue [] []

-- adds an item
push :: a -> Queue a -> Queue a
push x (Queue enc dec) =
  Queue (x : enc) dec

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue encs []) =
  case reverse encs of
    [] ->
      Nothing
    (x:xs) ->
      Just (x, Queue [] xs)
pop (Queue encs (x:xs)) =
  Just (x, Queue encs xs)

queueTests = do
  describe "queue" $ do
    it "emptyQ" $ do
      emptyQ `shouldBe` (Queue [] [] :: Queue Int)

    it "push" $ do
      push 1 emptyQ `shouldBe` (Queue [1] [] :: Queue Int)

    it "pop" $ do
      let q = push 3 $ push 2 $ push 1 emptyQ
      q `shouldBe` (Queue [3,2,1] [] :: Queue Int)

      let Just (1, q1) = pop q
      q1 `shouldBe` (Queue [] [2,3] :: Queue Int)

      let q2 = push 4 q1
      q2 `shouldBe` (Queue [4] [2,3] :: Queue Int)

      let Just (2, q3) = pop q2
      q3 `shouldBe` (Queue [4] [3] :: Queue Int)

      let Just (3, q4) = pop q3
      q4 `shouldBe` (Queue [4] [] :: Queue Int)

      let Just (4, q5) = pop q4
      q5 `shouldBe` (Queue [] [] :: Queue Int)

ghcid = do
  hspec $ do
    dlistTests
    queueTests
  putStrLn "fin ghcid 28."

main = do
  main'
  putStrLn "fin 28."
