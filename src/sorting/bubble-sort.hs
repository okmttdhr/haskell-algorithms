swap :: (Ord a) => [a] -> [a]
swap (x:y:xs)
  | x <= y    = x : swap (y:xs)
  | otherwise = y : swap (x:xs)
swap (x) = (x)

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs
  | swapped == xs = xs
  | otherwise     = bubbleSort swapped
  where swapped = swap xs
