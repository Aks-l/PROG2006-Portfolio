module GameCore
  ( apply
  , legal
  , floodFill
  , liberties
  , opp
  , neighbors
  ) where

import           Types
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Char            (ord, chr)



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
                    , gameFileName  = gameFileName g
                    }