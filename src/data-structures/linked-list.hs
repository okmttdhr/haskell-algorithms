data LinkedList a = Node a (LinkedList a) | Nil deriving (Show, Eq)

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList l = Node (head l) (fromList (tail l))
