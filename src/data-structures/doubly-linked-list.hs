data DoublyLinkedList a = Node (DoublyLinkedList a) a (DoublyLinkedList a)

-- (([9,3,4,12,99,33,44] 3 [4,12,99,33,44,9,3]) 4 ([4,12,99,33,44,9,3] 12 [99,33,44,9,3,4,12])) 
-- 12 
-- (([4,12,99,33,44,9,3] 12 [99,33,44,9,3,4,12]) 99 ([99,33,44,9,3,4,12] 33 [44,9,3,4,12,99,33]))
linked :: Integer -> [a] -> [a]
linked n xs | n < 0 = (last xs):(init xs)
            | n > 0 = tail xs ++ [head xs]

fromList :: [a] -> DoublyLinkedList a
fromList xs = Node (fromList $ linked (-1) xs) (head xs) (fromList $ linked 1 xs)

-- printRight 15 $ fromList [12,99,33,44,9,3,4]
-- => [12,99,33,44,9,3,4,12,99,33,44,9,3,4,12]
printRight :: Integer -> DoublyLinkedList a -> [a]
printRight 0 _ = []
printRight n (Node _ x next) = x : (printRight (n-1) next)

-- printLeft 15 $ fromList [12,99,33,44,9,3,4]
-- => [12,4,3,9,44,33,99,12,4,3,9,44,33,99,12]
printLeft :: Show a => Integer -> DoublyLinkedList a -> [a]
printLeft 0 _ = []
printLeft n (Node prev x _) = x : (printLeft (n-1) prev)
