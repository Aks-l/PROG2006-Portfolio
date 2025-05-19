module Lib
    ( countScore
    ) where

import Data.List (group, sort)


-- | Count the total score of the five balls lottery game data.
-- The input data is a text string with many games. Each game is represented as a single line.
-- The first three numbers are the "Winning Numbers", and the next five are the lottery numbers.
-- Each row ends with end-of-line character.
-- Write a function that uses the utility functions `processLine` to calculate the total score of the game.
countScore :: String -> Int
countScore = sum . map processLine . lines


-- | Process a single line of the input data.
-- Each line represents a game, the first three numbers are the "Winning Numbers", 
-- and the next five are the lottery numbers. Calculate the score for the single game.
--
-- >>> processLine "4 21 10 5 4 21 13 11"
-- 5
--
-- >>> processLine "4 21 10 5 4 21 13 11 10"
-- 7
--
-- >>> processLine "4 21 10 5 4 21 13 11 10 10"
-- 11
--
-- >>> processLine "10 21 10 5 4 21 13 11 10"
-- 8
--
-- >>> processLine "10 21 10 5 8 20 13 11"
-- 0
-- >>> processLine "10 10 10 5 4 21 13 11 10 10 10"
-- 56
--
-- >>> processLine "8 14 16 5 8 14 16 14"
-- 9
-- 
-- >>> processLine "8 14 16 5 8 18 16 12"
-- 3
--
-- >>> processLine "35 35 35 1 5 6 35 16"
-- 32
--
-- >>> processLine "35 35 6 1 5 6 35 35"
-- 49
-- 
-- >>> processLine "35 35 35 1 5 6 35 35"
-- 96
-- 

processLine :: String -> Int
processLine line = 
    let nums = read <$> words line 
        (winning, played) = splitAt 3 nums
        freqTuple = uniqFreq winning
        score = calculateScore freqTuple played
    in score 

-- Generate list of tuples (value, frequency) for unique values in winning numbers
uniqFreq :: [Int] -> [(Int, Int)]
uniqFreq = map (\x -> (head x, length x)) . group . sort

-- Compute total score based on winning number frequency
calculateScore :: [(Int, Int)] -> [Int] -> Int
calculateScore nums played = sum $ map score nums
  where
    score (num, freq) =
        let count = length (filter (== num) played) 
        in sum [scoreNum num * (2 ^ (occ + freq - 2)) | occ <- [1..count]]
        
-- Convert number to corresponding score
scoreNum :: Int -> Int
scoreNum =  (2^) . (`div` 10)

