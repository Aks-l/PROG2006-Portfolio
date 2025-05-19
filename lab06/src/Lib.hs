module Lib
    ( decodeMessage, decodeMessageImproved
    ) where


-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- Nothing
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- Nothing
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 8"
-- Just 3
-- 
-- >>> decodeMessage "5 5 5 1 2 3 4 8 2 3"
-- Nothing

decodeMessage :: String -> Maybe Int
decodeMessage msg =
  let nums = map read $ words msg in
  findUniqueLowest nums       >>= \lowest  ->
  findUniqueHighest nums      >>= \highest ->
  getMagic lowest highest     >>= \magic   ->
  return (length (filter (== magic) nums))

findUniqueLowest :: [Int] -> Maybe Int
findUniqueLowest nums =
  let lowest = minimum nums in
  if length (filter (== lowest) nums) == 1
    then Just lowest 
    else Nothing

findUniqueHighest :: [Int] -> Maybe Int
findUniqueHighest nums =
  let highest = maximum nums in
  if length (filter (== highest) nums) == 1
    then Just highest
    else Nothing

getMagic :: Int -> Int -> Maybe Int
getMagic lowest highest = 
    if even (lowest + highest) 
    then Just ((lowest + highest) `div` 2)
    else Nothing

-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.
-- This is an improved version of the previous function, with a more
-- informative error messages.
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- Left "Communication interference detected: minimum number not Unique"
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- Left "Communication interference detected: maximum number not Unique"
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 8"
-- Right 3
--
-- >>> decodeMessageImproved "5 5 5 1 2 3 4 8 2 3"
-- Left "Communication interference detected: midPoint not even"
decodeMessageImproved :: String -> Either String Int
decodeMessageImproved msg =
    let nums = map read $ words msg in
    improvedFindUniqueLowest nums       >>= \lowest  ->
    improvedFindUniqueHighest nums      >>= \highest ->
    improvedGetMagic lowest highest     >>= \magic   ->
    return (length (filter (== magic) nums))

improvedFindUniqueLowest :: [Int] -> Either String Int
improvedFindUniqueLowest nums =
    let lowest = minimum nums in
    if length (filter (== lowest) nums) == 1
        then Right lowest
        else Left "Communication interference detected: minimum number not Unique"

improvedFindUniqueHighest :: [Int] -> Either String Int
improvedFindUniqueHighest nums =
    let highest = maximum nums in
    if length (filter (== highest) nums) == 1
        then Right highest
        else Left "Communication interference detected: maximum number not Unique"

improvedGetMagic :: Int -> Int -> Either String Int
improvedGetMagic lowest highest =
    if even (lowest + highest)
        then Right ((lowest + highest) `div` 2)
        else Left "Communication interference detected: midPoint not even"