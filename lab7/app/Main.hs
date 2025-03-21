module Main (main) where

import Lib (countScore)
import Prelude

-- | Read the input data from stdin and print the total score of the game.
-- Note that the input data is read from stdin and the output is written to stdout.
-- We do not use do-notation here. The same code, in more similar imperative way, 
-- can be written as:
-- main = do
--   input <- getContents
--   putStrLn $ "The total score is: " ++ show (countScore input)
--
main :: IO ()
main = readFile "data.txt" >>= putStrLn . ("The total score is: " ++) . show . countScore
