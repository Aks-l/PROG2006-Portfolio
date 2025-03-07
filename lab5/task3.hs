-- | get the nth fibonacci number
-- >>> fibb 0
-- 0
-- >>> fibb 7
-- 13
-- >>> fibb 10
-- 55
fibb :: Integer -> Integer
fibb n
  | n < 0     = error "negative number"
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fibb (n-1) + fibb (n-2)
