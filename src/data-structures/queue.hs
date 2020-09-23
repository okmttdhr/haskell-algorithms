import Data.Maybe

data Queue a = Queue [a] deriving (Show)

-- push 2 (Queue [1])
-- => Queue [1,2]
push :: a -> Queue a -> Queue a
push x (Queue xs) = Queue (xs ++ [x])

-- pop (Queue [1,2])
-- => (Just 1,Queue [2])
-- pop (Queue [])
-- => (Nothing,Queue [])
pop :: Queue a -> (Maybe a, Queue a)
pop (Queue []) = (Nothing, Queue [])
pop (Queue (x:xs)) = (Just x, Queue xs)
