module GameCore
  ( apply
  , legal
  , floodFill
  , liberties
  , opp
  , neighbors
  , getBotMove
  , countTerritory
  ) where

import           Types
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Char            (ord, chr)
import           System.Random (randomRIO)
import Text.ParserCombinators.ReadP (get)
import           Data.List (maximumBy)
import           Data.Monoid ((<>))



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


-- | Get all empty points adjacent to any stone in the group
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
                    , komi      = komi g 
                    , gameFileName  = gameFileName g
                    , bot       = bot g
                    , bCaptured  = if col == Black
                        then bCaptured g + Set.size dead
                        else bCaptured g
                    , wCaptured  = if col == White
                        then wCaptured g + Set.size dead
                        else wCaptured g
                    }
    
countTerritory :: Int -> Map Point Color -> (Int,Int)
countTerritory size board = go Set.empty allEmpties (0,0)
  where
    -- every point in the NxN grid that has no stone
    allEmpties :: [Point]
    allEmpties =
      [ (x,y)
      | x <- [0..size-1]
      , y <- [0..size-1]
      , Map.notMember (x,y) board
      ]

    go :: Set Point -> [Point] -> (Int,Int) -> (Int,Int)
    go _    []     acc = acc
    go seen (p:ps) acc
      | p `Set.member` seen = go seen ps acc
      | otherwise =
          let region    = floodEmpty Set.empty [p]
              seen'     = seen `Set.union` region

              -- collect all stone‐colors adjacent to this empty region
              borders   = Set.fromList
                        [ c
                        | q <- Set.toList region
                        , n <- neighbors size q
                        , Just c <- [Map.lookup n board]
                        ]
              
              owner     = case Set.toList borders of
                            [Black] -> Just Black
                            [White] -> Just White
                            _       -> Nothing

              szRegion  = Set.size region
              acc'      = case owner of
                            Just Black -> (fst acc + szRegion, snd acc)
                            Just White -> (fst acc, snd acc + szRegion)
                            Nothing    -> acc
          in go seen' ps acc'

    -- flood‐fill starting on empty points (i.e. Map.notMember)
    floodEmpty :: Set Point -> [Point] -> Set Point
    floodEmpty region []     = region
    floodEmpty region (q:qs)
      | q `Set.member` region       = floodEmpty region qs
      | Map.member q board          = floodEmpty region qs
      | otherwise =
          let region' = Set.insert q region
              qs'     = neighbors size q ++ qs
          in floodEmpty region' qs'


getBotMove :: Color -> Game -> IO Move
getBotMove col g =
  let sz       = gameSize g
      b        = board g
      (tB, tW) = countTerritory sz b
      finalB   = tB + bCaptured g
      finalW   = tW + wCaptured g
      legalPts = [ p | p <- allPoints sz, legal p g ]
  in
  if passCount g == 1 && wins col (finalB, finalW)
    then return Pass
    else chooseRegularMove sz col b g legalPts
  where
    chooseRegularMove sz col b g moves = do
      case moves of
        [] -> do
            putStrLn "Bot: Pass"
            return Pass
        _  -> do
          r <- randomRIO (0.0,1.0 :: Double)
          if r < 0.2
            then do idx <- randomRIO (0, length moves - 1)
                    putStrLn $ "Bot: " ++ show (moves !! idx)
                    return $ Play (moves !! idx)
            else do
                let move = selectBestMove sz col b g moves
                putStrLn $ "Bot: " ++ show move
                return $ Play move

    wins Black (bScore, wScore) = bScore > wScore
    wins White (bScore, wScore) = wScore > bScore

    allPoints n = [ (x,y) | x <- [0..n-1], y <- [0..n-1] ]

-- | Pick the move that maximizes liberties and minimizes group count
selectBestMove :: Int -> Color -> Map Point Color -> Game -> [Point] -> Point
selectBestMove size col boardMap g moves =
  fst $ maximumBy compareScore
    [ (p, scoreAfter p) | p <- moves ]
  where
    compareScore (_, (lib1, grp1)) (_, (lib2, grp2)) =
      compare lib1 lib2 `mappend` compare grp2 grp1

    scoreAfter p =
      case apply (Play p) g of
        Nothing -> (minBound, maxBound)
        Just g' -> evaluateBoard size col (board g')

-- | Evaluate board for a color: (total liberties, number of groups)
evaluateBoard :: Int -> Color -> Map Point Color -> (Int, Int)
evaluateBoard size col boardMap =
  let groups = getGroups size col boardMap
      libSum = sum [ Set.size (liberties size grp boardMap) | grp <- groups ]
      grpCnt = length groups
  in (libSum, grpCnt)

-- | Get all connected groups of a given color
getGroups :: Int -> Color -> Map Point Color -> [Set Point]
getGroups size col boardMap = go Set.empty stones []
  where
    stones = [ p | (p,c) <- Map.toList boardMap, c == col ]
    go _    []     acc = acc
    go seen (p:ps) acc
      | p `Set.member` seen = go seen ps acc
      | otherwise           =
          let grp   = floodFill size p boardMap
              seen' = seen `Set.union` grp
          in go seen' ps (grp:acc)