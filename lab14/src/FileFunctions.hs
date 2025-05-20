module FileFunctions where


import Types (Game(..), Move(..), Color(..), Point)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.Regex.TDFA      ((=~))
import GameCore (apply)
import           Data.List.Split      (splitOn)
import           Control.Monad        (forM_, foldM)
import           Data.Char            (ord, chr)
import Control.Exception (evaluate)
import System.IO (withFile, hGetContents, hSeek, SeekMode(AbsoluteSeek), hPutStr, IOMode(..))


createGameFile :: String -> Double -> String -> IO ()
createGameFile boardSize komi time = do
  let fileName = time ++ ".sgf"
  writeFile fileName ("(;FF[4]CA[UTF-8]GM[1]SZ[" ++ boardSize ++ "]KM[" ++ show komi ++ "])")
  putStrLn $ "Game file created: " ++ fileName

updateGameFile :: String -> Move -> Color -> IO ()
updateGameFile gameFileName move col = do
  let color = case col of
               Black -> "B"
               White -> "W"
      m = gameNotation move
      fileName = gameFileName ++ ".sgf"

  stripped <- withFile fileName ReadMode $ \h -> do
    contents <- hGetContents h
    let forced = filter (/= ')') contents
    evaluate (length forced)  -- force full evaluation to release handle
    return forced

  let updated = stripped ++ ";" ++ color ++ "[" ++ m ++ "]\n)"

  withFile fileName WriteMode $ \h ->
    hPutStr h updated

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
  let content' = (init . filter (\c -> c /= '\r' && c /= ' ')) content
      moves = drop 1 (splitOn ";" content') :: [String]

      -- force the regex to be String->[[String]]
      sizeMatches :: [[String]]
      sizeMatches = content =~ ("SZ\\[([0-9]+)\\]" :: String)

      komiMatches :: [[String]]
      komiMatches = content =~ ("KM\\[([0-9]+(\\.[0-9]+)?)\\]" :: String)


  -- extract the one and only capture group
  size <- case sizeMatches of
    [[_, s]] -> Just (read s)
    _        -> Just 19  -- Default to size 19 if no match
  komi <- case komiMatches of
    [[_, k]] -> Just (read k)
    _        -> Just 6.5  -- Default to komi 6.5 if no match
  if size < 1 || size > 19 then Nothing else Just ()
  let fileName = take (length filePath - 4) filePath
      tempGame = Game
        { board     = Map.empty
        , toPlay    = White
        , history   = []
        , passCount = 0
        , gameSize  = size
        , komi      = komi
        , gameFileName  = fileName
        , bot      = Nothing
        , bCaptured = 0
        , wCaptured = 0
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
           }

