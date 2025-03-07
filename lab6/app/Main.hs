module Main (main) where

import Lib

main :: IO ()
main = do
    msg <- getContent
    case decodeMessage msg of
        Just n -> putStrLn $ "The message is " ++ show n ++ " characters long."
        Nothing -> putStrLn "The message cannot be decoded. Interference detected."
