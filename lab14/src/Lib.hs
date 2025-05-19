{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           System.Environment   (getArgs)
import           Text.Read            (readMaybe)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.IO            (hFlush, stdout, withFile, IOMode(ReadWriteMode), hFileSize, hSetFileSize)
import           Control.Monad        (forM_)


import Text.Printf (IsChar(toChar))
import FileFunctions (createGameFile, updateGameFile, parseSGF)
import Types (Game(..), Move(..), Color(..), Point)
import GameCore (apply, getBotMove, countTerritory)


-- | Get the current Unix time in seconds for file names
getUnixTime :: IO String
getUnixTime = show . round <$> getPOSIXTime

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
    ["q"] -> do
      enterInteractive g (length (history g) -1)
      getMove col g  
    ["pass"] -> return Pass
    [sx, sy] ->
      case (readMaybe sx, readMaybe sy) of
        (Just x, Just y)
          | x >= 0 && x < sz && y >= 0 && y < sz -> return (Play (x,y))
        _ -> invalid
    _ -> invalid
  where
    invalid = putStrLn "Invalid input" >> getMove col g
  
enterInteractive :: Game -> Int -> IO ()
enterInteractive game idx = do
  let maxIdx = length (history game) - 1
      validIdx = max 0 (min idx maxIdx)
      boardAt = reverse (history game) !! validIdx
  putStrLn $ "Move " ++ show validIdx ++ "/" ++ show maxIdx
  printBoard game { board = boardAt }
  putStrLn "Interactive mode. Type 'q' to quit, 'p' for previous, 'n' for next."
  command <- getLine
  case command of
    "q" -> return ()
    "p" -> enterInteractive game (validIdx - 1)
    "n" -> enterInteractive game (validIdx + 1)
    _   -> putStrLn "Invalid command" >> enterInteractive game validIdx


    -- | Ask whether to play vs. computer, and if so which color.
getBotColor :: IO (Maybe Color)
getBotColor =
  putStrLn "Play against the computer? [Y/n]" >>
  getLine >>= \botResp ->
  if botResp `elem` ["N","n"]
    then return Nothing
    else
      putStrLn "Play as color: [B]lack or [W]hite?" >>
      getLine >>= \colResp ->
        case colResp of
          "W" -> return (Just Black)
          "B" -> return (Just White)
          _   -> putStrLn "Invalid color, defaulting to Black." >>
                  return (Just White)

-- | Main game loop
gameLoop :: Game -> IO ()
gameLoop g = do
  putStrLn ""
  if passCount g >= 2
    then endGame g
    else do
      printBoard g

      mv <- case bot g of
        Just botCol | botCol == toPlay g -> getBotMove botCol g
        _ -> getMove (toPlay g) g

      case apply mv g of
        Nothing -> putStrLn "Illegal move!" >> gameLoop g
        Just g' -> do
          updateGameFile (gameFileName g) mv (toPlay g)
          gameLoop g'

-- | Entry point
gameEntry :: IO ()
gameEntry = do
  args <- getArgs
  case args of
    (filepath:_) -> do
      content <- readFile filepath
      case parseSGF content filepath of
        Just game -> gameLoop game
        Nothing   -> putStrLn "Invalid SGF file" >> return ()
    [] -> do
      putStrLn "Enter board size (1-19):"
      boardSize <- getLine
      case readMaybe boardSize of
        Just sz | sz > 0 && sz <= 19 ->
          getUnixTime   >>= \boardTime ->
          getBotColor   >>= \bot ->
          createGameFile boardSize boardTime >>
          gameLoop Game
            { board        = Map.empty
            , toPlay       = Black
            , history      = []
            , passCount    = 0
            , gameSize     = sz
            , gameFileName = boardTime
            , bot          = bot
            , bCaptured    = 0
            , wCaptured    = 0
            }
        _ -> putStrLn "Invalid size, must be between 1 and 19." >>
             gameEntry
          
-- | End game
endGame :: Game -> IO ()
endGame g = do
  let (bTerritory, wTerritory) = countTerritory (gameSize g) (board g)
  putStrLn "Game ended!"
  putStrLn $ "Black score: " ++ show bTerritory ++ " + " ++ show (bCaptured g) ++ " = " ++ show (bTerritory + bCaptured g)
  putStrLn $ "White score: " ++ show wTerritory ++ " + " ++ show (wCaptured g) ++ " = " ++ show (wTerritory + wCaptured g)
  putStrLn $ "Winner: " ++ (if bTerritory + bCaptured g > wTerritory + wCaptured g then "Black" else "White")

 