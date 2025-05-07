module Types where

import          Data.Map.Strict      (Map) 
import Data.Ord (Ord)

type Point = (Int,Int)

data Color = Black | White
  deriving (Eq, Show, Ord)

data Move = Play Point | Pass
  deriving (Eq, Show)

data Game = Game
  { board     :: Map Point Color
  , toPlay    :: Color
  , history   :: [Map Point Color]   -- for simple ko
  , passCount :: Int
  , gameSize  :: Int
  , gameFileName  :: String
  , bot :: Maybe Color
  , bCaptured :: Int
  , wCaptured :: Int
  }
