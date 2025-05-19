module Main (main) where

import Lib (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    putStrLn "Running on http://localhost:8080/"
    run 8080 app
