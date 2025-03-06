fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t

fib2 :: Int -> Integer
fib2 n = fibs!!n


