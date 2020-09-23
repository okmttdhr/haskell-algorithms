import Data.Maybe

data Stack a = Stack [a] deriving Show

-- push 2 (Stack [1])
-- => Stack [2,1]
push :: a -> Stack a -> Stack a
push x (Stack xs)= Stack (x:xs)

-- pop (Stack [2,1])
-- => (Just 2,Stack [1])
-- pop (Stack [])
-- => (Nothing,Stack [])
pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)
