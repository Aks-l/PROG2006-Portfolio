-- | get the nth fibonacci number
-- >>> fibb2 4
-- 3
-- >>> fibb2 7
-- 13
-- >>> fibb2 15
-- 610
fibs :: [Integer]
fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t

fib2 :: Int -> Integer
fib2 n = fibs!!n

