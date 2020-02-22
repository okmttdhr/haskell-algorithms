quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
  let smallerOrEqual = filter (<= x) xs
      larger         = filter (> x) xs
  in quickSort smallerOrEqual ++ [x] ++ quickSort larger
