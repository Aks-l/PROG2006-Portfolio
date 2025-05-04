module FileFunctions where


import Types (Game(..), Move(..), Color(..), Point)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.Regex.TDFA      ((=~))
import GameCore (apply)
import           Data.List.Split      (splitOn)
import           Control.Monad        (forM_, foldM)
import           Data.Char            (ord, chr)
import           System.IO            (hFlush, stdout, withFile, IOMode(ReadWriteMode), hFileSize, hSetFileSize)


createGameFile :: String -> String -> IO ()
createGameFile boardSize time = do
  let fileName = time ++ ".sgf"
  writeFile fileName ("(;FF[4]CA[UTF-8]GM[1]SZ[" ++ boardSize ++ "]\n")
  putStrLn $ "Game file created: " ++ fileName

updateGameFile :: String -> Move -> Color -> IO ()
updateGameFile gameFileName move col = do
  let color = case col of
               Black -> "B"
               White -> "W"
      m = gameNotation move
      fileName = gameFileName ++ ".sgf"
  
  truncateLastByte fileName
  appendFile fileName $ ";" ++ color ++ "[" ++ m ++ "]\n" 
  endGameFile gameFileName

truncateLastByte :: FilePath -> IO ()
truncateLastByte path =
  withFile path ReadWriteMode $ \h -> do
    sz <- hFileSize h
    if sz > 0
      then hSetFileSize h (sz - 1)
      else return ()
  
endGameFile :: String -> IO ()
endGameFile gameFileName = do
  let fileName = gameFileName ++ ".sgf"
  appendFile fileName ")"

gameNotation :: Move -> String
gameNotation Pass = ""
gameNotation (Play (x,y)) = [toLetter x, toLetter y] 

toLetter :: Int -> Char
toLetter n = chr (ord 'a' + n)

toInt :: Char -> Int
toInt c = ord c - ord 'a'

parseSGF :: String -> String -> Maybe Game
parseSGF content filePath = do
  -- split off the header “(;…” and the trailing “)”
  let content' = (init . filter (/= '\r')) content
      moves = drop 1 (splitOn ";" content') :: [String]

      -- force the regex to be String->[[String]]
      sizeMatches :: [[String]]
      sizeMatches = content =~ ("SZ\\[([0-9]+)\\]" :: String)

  -- extract the one and only capture group
  [[_, s]] <- pure sizeMatches
  let size    = read s
      fileName = take (length filePath - 4) filePath
      tempGame = Game
        { board     = Map.empty
        , toPlay    = Black
        , history   = []
        , passCount = 0
        , gameSize  = size
        , gameFileName  = fileName
        }

  -- foldM steps through each token
  foldM step tempGame moves

 where
  step :: Game -> String -> Maybe Game
  step g m =
    let match :: [[String]]
        match = m =~ ("^([BW])\\[([a-z])([a-z])\\]$" :: String)
    in case match of
         -- each of col, x, y is a [Char], but we pattern‐match to single‐char strings
         [[_, col, [cx], [cy]]] ->
           apply (Play (toInt cx, toInt cy)) g
         _ ->
           Just g {
              toPlay    = if toPlay g == Black then White else Black
           }   -- ignore anything that doesn’t look like B[aa], W[dp], etc.

