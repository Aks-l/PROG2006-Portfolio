
main :: IO ()
main = do
    putStrLn "Hi, what is your name?"
    name <- getLine
    putStrLn "and what is your age?"
    age <- getLine
    putStrLn ("Hello, " ++ name ++ " in 10 years you will be " ++ show (read age + 10) ++ " years old.")

    print $ addAge 10 (Age 20)



newtype Age = Age Int
    deriving (Eq, Num, Show)
addAge :: Age -> Age -> Age
addAge a b = a + b


addNumber :: Int -> Int -> Int
addNumber a b = a + b

listToValue :: [a] -> a
listToValue [x] = x



mhead1 :: [a] -> a
mhead1 a = a!!0

mhead2 :: [a] -> a
mhead2 (x:_) = x

mhead3 :: [a] -> a
mhead3 a = last $ reverse a

mhead4 :: [a] -> a
mhead4 x = listToValue (take 1 x)

mhead5 :: [a] -> a
mhead5 = head
