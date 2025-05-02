module Main where

import System.IO (hSetEncoding, stdin, stdout, utf8)
import Lib (gameEntry)

main ::  IO ()
main = do
    hSetEncoding stdin  utf8
    hSetEncoding stdout utf8
    gameEntry