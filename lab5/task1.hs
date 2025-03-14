-- | get head of a list
-- >>> mhead1 [1,2,3]
-- 1
-- >>> mhead1 "Hello"
-- 'H'
mhead1 :: [a] -> a
mhead1 [] = error "empty list"
mhead1 (a:_) = a

-- | get head of a list
-- >>> mhead2 [1,2,3]
-- 1
-- >>> mhead2 "Hello"
-- 'H'
mhead2 :: [a] -> a
mhead2 a
    | null a = error "empty list"
    | otherwise = a !! 0

-- | get head of a list
-- >>> mhead3 [1,2,3]
-- 1
-- >>> mhead3 "Hello"
-- 'H'
mhead3 :: [a] -> a
mhead3 a = 
    if null a 
    then error "empty list" 
    else a !! 0

-- | get head of a list
-- >>> mhead4 [1,2,3]
-- 1
-- >>> mhead4 "Hello"
-- 'H'
mhead4 :: [a] -> a
mhead4 a = 
    let (x:_) = a
    in x

-- | get head of a list
-- >>> mhead5 [1,2,3]
-- 1
-- >>> mhead5 "Hello"
-- 'H'
mhead5 :: [a] -> a
mhead5 a = x
    where (x:_) = a

-- | get head of a list
-- >>> mhead6 [1,2,3]
-- 1
-- >>> mhead6 "Hello"
-- 'H'
mhead6 :: [a] -> a
mhead6 a = case a of
    [] -> error "empty list"
    (x:_) -> x
