mhead1 :: [a] -> a
mhead1 [] = error "empty list"
mhead1 (a:_) = a

mhead2 :: [a] -> a
mhead2 a
    | null a = error "empty list"
    | otherwise = a!!0

mhead3 :: [a] -> a
mhead3 a = if null a then error "empty list" else a!!0

mhead4 :: [a] -> a
mhead4 a = 
    let (x:_) = a
    in x

mhead5 :: [a] -> a
mhead5 a = x
    where (x:_) = a

mhead6 :: [a] -> a
mhead6 a = case a of
    [] -> error "empty list"
    (x:_) -> x
