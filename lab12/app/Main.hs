module Main (main) where

import Lib   (entry)
import Types (Stack)
import System.IO (hSetEncoding, stdin, stdout, utf8)
main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  -- start with an empty stack
  let initialStack :: Stack
      initialStack = []
  entry initialStack
