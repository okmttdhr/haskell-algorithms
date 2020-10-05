data MinHeap a = Node a (MinHeap a) (MinHeap a) Int | Leaf deriving Show

fromList :: Ord a => [a] -> MinHeap a
fromList = foldr add empty

empty :: MinHeap a
empty = Leaf

heapLength :: MinHeap a -> Int
heapLength Leaf = 0
heapLength (Node _ _ _ l) = l

add :: Ord a => a -> MinHeap a -> MinHeap a
add v Leaf = Node v Leaf Leaf 1
add v (Node e l r len)
    | v < e               = add e (Node v l r len)
    | heapLength l < heapLength r = Node e (add v l) r (len+1)
    | otherwise           = Node e l (add v r) (len+1)
