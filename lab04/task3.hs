main :: IO ()
main = do
  input <- getContents
  let ages = map (read . last . words) (lines input) :: [Int]
      update (max, freq) age
        | age > max   = (age, 1)
        | age == max  = (max, freq + 1)
        | otherwise   = (max, freq)
      (maxAge, count) = foldl update (0, 0) ages
  print count

{-

main :: IO ()
main = do
    input <- getContents 
    let parsed = lines input
        splitted = map words parsed
        ages = map (read . last) splitted :: [Int]
        oldest = maximum ages
        freq = length $ [x | x <- ages, x == oldest]
    print freq
-}