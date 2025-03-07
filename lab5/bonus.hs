fibWithZip :: Int -> Integer
fibWithZip n = 
    let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    in fibs !! n

