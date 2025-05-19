describeList :: (Show a) => [a] -> String
describeList [] = "The list is empty."
describeList xs@(x:_) =
  "The list starts with " ++ show x ++ " and has " ++ show (length xs) ++ " elements."
