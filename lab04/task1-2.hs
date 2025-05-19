mreverse :: [a] -> [a]
mreverse [] = []
mreverse (x:xs) = mreverse xs ++ [x]

-- | mreverse is my own implementation of list reversal
--
-- >>> mreverse "Hello"
-- "olleH"
--
-- >>> mreverse [1,2,3]
-- [3,2,1]


padding :: Int -> String
padding x
  | x < 10 = "   " ++ show x
  | x < 100 = "  " ++ show x
  | otherwise = " " ++ show x

mulTable :: Int -> IO ()
mulTable n = do
    let table = [ [i*j | i <- [1..n]] | j <- [1..n] ]
    mapM_ (putStrLn . unwords . map padding) table


{-
maxage=0
out=1
for age in ages:
    if age>maxage: out=1
    elif age==maxage: out+=1
-}

