{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data GreetStruct = 
  GreetStruct 
    { msg  :: String
    , name :: String
    } deriving (Show, Generic)
instance ToJSON GreetStruct

data GreetMeStruct = 
  GreetMeStruct 
    { input :: String
    , name  :: String
    } deriving (Show, Generic)
instance FromJSON GreetMeStruct

data GreetMeResponse = 
  GreetMeResponse 
    { msg :: String
    } deriving (Show, Generic)
instance ToJSON GreetMeResponse
