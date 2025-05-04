module Types where

import          Data.Map.Strict      (Map) 

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
  , gameFileName  :: String
  , bot :: Maybe Color
  }
