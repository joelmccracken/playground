

allspots :: [(Integer, Integer)]
allspots = [(i,j)| i <- [0..19], j <- [0..19]]


hasDown (_,b) = b <= 16
hasLeft (a,_) = a <= 16
hasRiag a = hasDown a && hasLeft a
hasLiag (a,b) = a >3 && hasDown (a,b)


downTrans (a,b) = (a,b+1)
leftTrans (a,b) = (a+1,b)
riagTrans (a,b) = (a+1,b+1)
liagTrans (a,b) = (a-1,b+1)


getDown a g = getMany a g downTrans 4
getLeft a g = getMany a g leftTrans 4
getRiag a g = getMany a g riagTrans 4
getLiag a g = getMany a g liagTrans 4


getMany :: (Integer,Integer) -> [[Integer]] -> ((Integer,Integer)->(Integer,Integer))->Integer->Integer
getMany a g trans 0 = 1
getMany a g trans n = (getSpot a g) * (getMany (trans a) g trans (n-1))


getSpot :: (Integer, Integer) -> [[Integer]] -> Integer
getSpot (a,b) g = (g !! (fromIntegral b)) !! (fromIntegral a)

leftVal :: (Integer, Integer) -> [[Integer]] -> Integer
leftVal a g = if (hasLeft a) then (getLeft a g) else 0 
downVal :: (Integer, Integer) -> [[Integer]] -> Integer
downVal a g = if (hasDown a) then (getDown a g) else 0
riagVal :: (Integer, Integer) -> [[Integer]] -> Integer
riagVal a g = if (hasRiag a) then (getRiag a g) else 0
liagVal a g = if (hasLiag a) then (getLiag a g) else 0

lmax :: [Integer] -> Integer
lmax = foldl max 0 

gridVal :: [[Integer]] -> (Integer, Integer) -> Integer
gridVal g a = lmax [(leftVal a g), (downVal a g), (riagVal a g), (liagVal a g)]

readInteger :: String -> Integer
readInteger = read

readGrid :: String -> [[Integer]]
readGrid g = map (\x-> map readInteger $ words x) $ lines g

main = do
  x <- readFile "problem11.txt"
  print . lmax $ map (gridVal (readGrid x)) allspots
  --print $ map (gridVal (readGrid x)) allspots
