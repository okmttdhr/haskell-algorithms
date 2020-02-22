import Data.List (minimum, delete)

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = x : selectionSort (delete x xs)
  where x = minimum xs
