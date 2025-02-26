import Text.Printf

mreverse :: [a] -> [a]
mreverse [] = []
mreverse (x:xs) = mreverse xs ++ [x]


mulTable :: Int -> IO ()
mulTable n = do
    let table = [ [i*j | i <- [1..n]] | j <- [1..n] ]
    mapM_ (putStrLn . unwords . map (printf "%3d")) table

