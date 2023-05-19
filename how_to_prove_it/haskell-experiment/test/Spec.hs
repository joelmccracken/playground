import Test.Hspec
-- import Control.Monad

gen1 :: [Bool]
gen1  = [False, True]

gen2 :: [(Bool, Bool)]
gen2 = gen1 >>= \a -> gen1 >>= \b -> pure (a, b)

gen3 :: [(Bool, Bool, Bool)]
gen3 = gen2 >>= \(a,b) -> gen1 >>= \c -> pure (a, b, c)

main :: IO ()
main = do
  hspec $ do
    it "does this example" $ do
      let l (a, b, c) = a && b && c
      let r (a, b, c) = c && b && a
      l <$> gen3 `shouldMatchList` r <$> gen3
    it "problem 14b" $ do
      let l z@(a, b, c) =
            (((a && b) && not c) || (c && not (a && b))
            , z
            )
      let r z@(a, b, c) =
            ((((a && not c) || (c && not a)) && not (a && not b)) ||
             ((a && not b) && not ((a && not c) || c && not a))
            , z
            )
      l <$> gen3 `shouldMatchList` r <$> gen3
