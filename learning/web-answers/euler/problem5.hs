module Problem5 (primes, seive, factor) where
import List
import Data.Function


primes :: [Integer]
primes = seive [2..]

seive (p:xs) = p : seive [x | x <- xs, x `mod` p > 0]
seive p = p


problem5 = foldl (*) 1 . foldl (++) [] . 
	       map (maximumBy (compare `on` length)) .
	       groupBy ((==) `on` head).	
	       sortBy (compare `on` head) .	
	       nub . foldl (++) [] . map (\x-> group $ factor x)

factor :: Integer -> [Integer]
factor 1 = []
factor n = case find (\x->n `mod` x == 0) primes of
	Nothing -> [] -- this will never happen -- wil always find a composite
			-- i wonder how this is supposed to be handled 
			-- in haskell?
	Just x -> x : (factor (n `div` x))

main = do
  print $ problem5 [1..20]
