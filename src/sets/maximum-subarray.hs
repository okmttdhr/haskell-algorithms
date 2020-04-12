maximumSubarray' :: Int -> Int -> Int -> Int -> Int -> [Int] -> (Int, Int, Int)
maximumSubarray' maxSum currentSum currentIndex startIndex endIndex []     = (maxSum, startIndex, endIndex)
maximumSubarray' maxSum currentSum currentIndex startIndex endIndex (x:xs)
  | maxSum < currentSum' = maximumSubarray' currentSum' currentSum' currentIndex' startIndex    currentIndex  xs
  | currentSum' < 0      = maximumSubarray' maxSum      0           currentIndex' currentIndex' currentIndex' xs
  | otherwise            = maximumSubarray' maxSum      currentSum' currentIndex' startIndex    endIndex      xs
  where currentIndex' = currentIndex + 1 
        currentSum'   = currentSum + x

maximumSubarray :: [Int] -> (Int, Int, Int)
maximumSubarray = maximumSubarray' 0 0 0 0 0
