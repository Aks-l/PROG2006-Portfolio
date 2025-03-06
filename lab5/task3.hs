fibb :: Integer -> Integer
fibb n
    | n < 0     = error "requires non-negative integer"
    | otherwise = go n 0 1
  where
    go 0 a _ = a
    go n a b = go (n - 1) b (a + b)
