merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

half :: [a] -> ([a], [a])
half xs = (take n xs, drop n xs)
  where n = length xs `div` 2 

mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs 
  | length xs < 2 = xs
  | otherwise     = merge (mergeSort ls) (mergeSort rs)
  where (ls, rs) = half xs
