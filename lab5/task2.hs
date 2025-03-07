factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1..n]

-- | Calculate the factorial of a number
-- >>> factorial 0
-- 1
-- >>> factorial 5
-- 120
-- >>> factorial 10
-- 3628800