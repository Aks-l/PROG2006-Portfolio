factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1..n]
-- factorial n = product [1..n]

