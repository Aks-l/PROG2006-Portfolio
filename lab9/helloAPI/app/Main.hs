module Main where

import Network.Wai.Handler.Warp (run)
import Server (app)  -- Import `app` from `Lib.hs`

main :: IO ()
main = do
    putStrLn "Running on http://localhost:8080/"
    run 8080 app
