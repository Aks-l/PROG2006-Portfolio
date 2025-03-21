{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib (app) where

import Network.Wai (Application)
import Servant
import Servant.Client
import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:), (.:?))
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Token (gitlabToken)

-- Author data type
data Author =
    Author
      { id       :: Int
      , username :: String
      , name     :: String
      , state    :: String
      , web_url  :: String
      } deriving (Show, Generic)

instance FromJSON Author where
  parseJSON = withObject "Author" $ \v ->
    Author <$> v .: "id"
           <*> v .: "username"
           <*> v .: "name"
           <*> v .: "state"
           <*> v .: "web_url"

instance ToJSON Author

-- Issue data type
data Issue =
    Issue
      { id         :: Int
      , iid        :: Int
      , project_id :: Int
      , title      :: String
      , state      :: String
      , created_at :: String
      , updated_at :: String
      , closed_at  :: Maybe String
      , labels     :: [String]
      , author     :: Author
      , web_url    :: String
      } deriving (Show, Generic)

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \v ->
    Issue <$> v .: "id"
          <*> v .: "iid"
          <*> v .: "project_id"
          <*> v .: "title"
          <*> v .: "state"
          <*> v .: "created_at"
          <*> v .: "updated_at"
          <*> v .:? "closed_at"
          <*> v .: "labels"
          <*> v .: "author"
          <*> v .: "web_url"

instance ToJSON Issue

-- Define the GitLab API using Servant
type GitLabAPI =
       "api" :> "v4" :> "projects" :> Capture "projectId" Int
             :> "issues"
             :> QueryParam "labels" String
             :> QueryParam "state" String
             :> Header "PRIVATE-TOKEN" String
             :> Get '[JSON] [Issue]

-- Generate the client function
getGitLabIssues :: Int -> Maybe String -> Maybe String -> Maybe String -> ClientM [Issue]
getGitLabIssues = client (Proxy :: Proxy GitLabAPI)

-- Our local API which serves issues
type API = "issues" :> Get '[JSON] [Issue]

server :: Server API
server = issueHandler

issueHandler :: Handler [Issue]
issueHandler = do
  issues <- liftIO getIssues
  case issues of
    Left err -> throwError err500 { errBody = L8.pack err }
    Right is -> return is

-- Call the GitLab API using Servant.Client
getIssues :: IO (Either String [Issue])
getIssues = do
  manager <- newManager tlsManagerSettings 
  let baseUrl   = BaseUrl Https "git.gvk.idi.ntnu.no" 443 ""
      projectId = 5881
      labels    = Just "Task Check Request"
      state     = Just "opened"
      token     = Just gitlabToken
  res <- runClientM (getGitLabIssues projectId labels state token) (mkClientEnv manager baseUrl)
  return $ either (Left . show) Right res

app :: Application
app = serve (Proxy :: Proxy API) server
