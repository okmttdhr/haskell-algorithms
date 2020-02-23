data Tree a = Empty | Node !a !(Tree a) !(Tree a)

makeTree :: [a] -> Tree a
makeTree xs = fst $ makeTree' (length xs) xs
makeTree' :: Int -> [a] -> (Tree a, [a])
makeTree' 0 xs     = (Empty, xs)
makeTree' n (x:xs) = (Node x l r, zs)
  where
    m = n `div` 2
    (l, ys) = makeTree' m           xs
    (r, zs) = makeTree' (n - m - 1) ys

makeHeap :: Ord a => Tree a -> Tree a
makeHeap Empty        = Empty
makeHeap (Node x l r) = downHeap x (makeHeap l) (makeHeap r)
downHeap :: Ord a => a -> Tree a -> Tree a -> Tree a
downHeap x Empty Empty = Node x Empty Empty
downHeap x (Node y _ _) Empty
  | y < x     = Node y (Node x Empty Empty) Empty
  | otherwise = Node x (Node y Empty Empty) Empty
downHeap x l@(Node y ll lr) r@(Node z rl rr)
  | y < x && not (z < y) = Node y (downHeap x ll lr) r
  | z < x &&      z < y  = Node z l (downHeap x rl rr)
  | otherwise            = Node x l r

toList :: Ord a => Tree a -> [a]
toList Empty        = []
toList (Node x l r) = x:merge (toList l) (toList r)
merge :: Ord a => [a] -> [a] -> [a]
merge l [] = l
merge [] r = r
merge l@(x:xs) r@(y:ys)
  | y < x     = y:merge l ys
  | otherwise = x:merge xs r

heapSort :: Ord a => [a] -> [a]
heapSort = toList . makeHeap . makeTree
