
-- | get head
-- >>> mhead1 "hello"
-- 'h'
-- >>> mhead1 [1,2,3]
-- 1
mhead1 :: [a] -> a
mhead1 a = a!!0

-- | get head
-- >>> mhead2 "hello"
-- 'h'
-- >>> mhead2 [1,2,3]
-- 1

mhead2 :: [a] -> a
mhead2 (x:_) = x

-- | get head
-- >>> mhead3 "hello"
-- 'h'
-- >>> mhead3 [1,2,3]
-- 1

mhead3 :: [a] -> a
mhead3 a = last $ reverse a

listToValue :: [a] -> a
listToValue [x] = x


-- | get head
-- >>> mhead4 "hello"
-- 'h'
-- >>> mhead4 [1,2,3]
-- 1
mhead4 :: [a] -> a
mhead4 x = listToValue (take 1 x)

-- | get head
-- >>> mhead5 "hello"
-- 'h'
-- >>> mhead5 [1,2,3]
-- 1
mhead5 :: [a] -> a
mhead5 = head


