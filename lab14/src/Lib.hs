{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad        (forM_)
import           Data.Char            (ord, chr)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           System.Environment   (getArgs)
import           System.IO            (hFlush, stdout)
import           Text.Read            (readMaybe)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Printf (IsChar(toChar))

-- | Board coordinates 0–(size-1)
type Point = (Int,Int)

data Color = Black | White
  deriving (Eq, Show)

data Move = Play Point | Pass
  deriving (Eq, Show)

data Game = Game
  { board     :: Map Point Color
  , toPlay    :: Color
  , history   :: [Map Point Color]   -- for simple ko
  , passCount :: Int
  , gameSize  :: Int
  , gameTime  :: String
  }

-- | Get the current Unix time in seconds for file names
getUnixTime :: IO String
getUnixTime = show . round <$> getPOSIXTime

-- | Find adjacent neighbor points
neighbors :: Int -> Point -> [Point]
neighbors size (x,y) =
  filter onBoard [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
  where onBoard (i,j) = i>=0 && i<size && j>=0 && j<size

-- | Flood‐fill to get the entire group at p
floodFill :: Int -> Point -> Map Point Color -> Set Point
floodFill size p b = go Set.empty [p]
  where
    Just c   = Map.lookup p b
    go seen []     = seen
    go seen (q:qs)
      | q `Set.member` seen         = go seen qs
      | Map.lookup q b /= Just c    = go seen qs
      | otherwise =
          let ns = neighbors size q
          in go (Set.insert q seen) (ns ++ qs)

-- | All empty points adjacent to any stone in the group
liberties :: Int -> Set Point -> Map Point Color -> Set Point
liberties size grp b =
  Set.fromList
    [ n
    | p <- Set.toList grp
    , n <- neighbors size p
    , Map.notMember n b
    ]

-- | Opponent color
opp :: Color -> Color
opp Black = White
opp White = Black

-- | Is playing at p legal?
legal :: Point -> Game -> Bool
legal p g
  | Map.member p b            = False
  | suicide && noCapture      = False
  | newBoard `elem` history g = False
  | otherwise                 = True
  where
    b        = board g
    color    = toPlay g
    sz       = gameSize g
    b'       = Map.insert p color b

    enemyNs  = [ n | n <- neighbors sz p, Map.lookup n b == Just (opp color) ]
    captured = Set.unions
      [ if Set.null (liberties sz (floodFill sz n b') b')
          then floodFill sz n b'
          else Set.empty
      | n <- enemyNs
      ]

    newBoard = Set.foldr Map.delete b' captured

    myGroup  = floodFill sz p b'
    libs     = liberties sz myGroup b'
    suicide  = Set.null libs

    noCapture = all (\n ->
                      let grp = floodFill sz n b'
                      in not (Set.null (liberties sz grp b'))
                    ) enemyNs

-- | Apply a move (assumes legal or it's a Pass)
apply :: Move -> Game -> Maybe Game
apply Pass g =
  Just g { toPlay    = opp (toPlay g)
         , history   = board g : history g
         , passCount = passCount g + 1
         }

apply (Play p) g
  | not (legal p g) = Nothing
  | otherwise       =
      let col     = toPlay g
          b       = board g
          sz      = gameSize g
          b'      = Map.insert p col b
          enemyNs = [ n | n <- neighbors sz p, Map.lookup n b == Just (opp col) ]
          dead    = Set.unions
                    [ if Set.null (liberties sz (floodFill sz n b') b')
                        then floodFill sz n b'
                        else Set.empty
                    | n <- enemyNs
                    ]
          clean   = foldr Map.delete b' dead
      in Just Game { board     = clean
                    , toPlay    = opp col
                    , history   = b : history g
                    , passCount = 0
                    , gameSize  = sz
                    , gameTime  = gameTime g
                    }

-- | ASCII stones: ○=Black, ●=White, +=empty
stoneChar :: Map Point Color -> Point -> Char
stoneChar b p = case Map.lookup p b of
  Just Black -> '○'
  Just White -> '●'
  Nothing    -> '+'

-- | Print board using dynamic size
printBoard :: Game -> IO ()
printBoard g = do
  let b   = board g
      sz  = gameSize g
  forM_ [sz-1,sz-2..0] $ \y -> do
    putStr $ (if y < 10 then "  " else " ") ++ show y
    forM_ [0..sz-1] $ \x ->
      putStr $ "  " ++ [stoneChar b (x,y)]
    putStrLn ""
  putStr "   "
  forM_ [0..sz-1] $ \x ->
    putStr $ (if x < 10 then "  " else " ") ++ show x
  putStrLn ""

-- | Get a move from the user within the size of the board
getMove :: Color -> Game -> IO Move
getMove col g = do
  let sz = gameSize g
  putStr (show col ++ " to play> ")
  hFlush stdout
  line <- getLine
  case words line of
    ["pass"] -> return Pass
    [sx, sy] ->
      case (readMaybe sx, readMaybe sy) of
        (Just x, Just y)
          | x >= 0 && x < sz && y >= 0 && y < sz -> return (Play (x,y))
        _ -> invalid
    _ -> invalid
  where
    invalid = putStrLn "Invalid input" >> getMove col g

-- | Main game loop
gameLoop :: Game -> IO ()
gameLoop g = do
  putStrLn ""
  if passCount g >= 2
    then endGame g
    else do
      printBoard g
      mv <- getMove (toPlay g) g
      case apply mv g of
        Nothing -> putStrLn "Illegal move!" >> gameLoop g
        Just g' -> do 
          updateGameFile (gameTime g) mv (toPlay g)
          gameLoop g'

-- | Entry point
gameEntry :: IO ()
gameEntry = do
  args <- getArgs
  case args of
    (filepath:_) -> do
      content <- readFile filepath
      case parseSGF content of
        Just game -> gameLoop game
        Nothing   -> putStrLn "Invalid SGF file" >> return ()
    [] -> do
      putStrLn "Enter board size (1-19):"
      boardSize <- getLine
      boardTime <- getUnixTime
      createGameFile boardSize boardTime
      case readMaybe boardSize of
        Just sz | sz > 0 && sz <= 19 ->
          gameLoop Game { board     = Map.empty
                        , toPlay    = Black
                        , history   = []
                        , passCount = 0
                        , gameSize  = sz
                        , gameTime  = boardTime
                        }
        _ -> putStrLn "Invalid size; enter a number 1-19." >> gameEntry

-- | End game
endGame :: Game -> IO ()
endGame g = do
  let blackScore = Map.size $ Map.filter (== Black) (board g)
      whiteScore = Map.size $ Map.filter (== White) (board g)
  endGameFile (gameTime g)
  putStrLn "Game ended!"
  putStrLn $ "Black stones: " ++ show blackScore
  putStrLn $ "White stones: " ++ show whiteScore

createGameFile :: String -> String -> IO ()
createGameFile boardSize time = do
  let fileName = "game_" ++ time ++ ".sgf"
  writeFile fileName ("(;FF[4]CA[UTF-8]GM[1]SZ[" ++ boardSize ++ "]\n")
  putStrLn $ "Game file created: " ++ fileName

updateGameFile :: String -> Move -> Color -> IO ()
updateGameFile gameTime Pass _ = do
  pure ()
updateGameFile gameTime move col = do
  let color = case col of
               Black -> "B"
               White -> "W"
      m = gameNotation move
      fileName = "game_" ++ gameTime ++ ".sgf"
  
  appendFile fileName $ ";" ++ color ++ "[" ++ m ++ "]\n" 

endGameFile :: String -> IO ()
endGameFile gameTime = do
  let fileName = "game_" ++ gameTime ++ ".sgf"
  appendFile fileName ")"

gameNotation :: Move -> String
gameNotation (Play (x,y)) = [toLetter x, toLetter y] 

toLetter :: Int -> Char
toLetter n = chr (ord 'a' + n)

toInt :: Char -> Int
toInt c = ord c - ord 'a'

-- | SGF parsing placeholder
parseSGF :: String -> Maybe Game
parseSGF _ = Nothing
