data LinkedList a = Cons a (LinkedList a) | Nil deriving (Show, Eq)

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList l = Cons (head l) (fromList (tail l))
