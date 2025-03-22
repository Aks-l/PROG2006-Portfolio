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

-- For HTML generation
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Servant.HTML.Blaze (HTML)
import GHC.Stack (ccSrcSpan)


-------- TYPE DEFINITIONS --------

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

-------- API, SERVER AND APP --------
type API = "issues" :> Get '[JSON] [Issue]
      :<|> "queue"  :> Get '[HTML] H.Html
      :<|> "static" :> Raw

server :: Server API
server = issueHandler 
    :<|> queueHandler
    :<|> serveDirectoryFileServer "static"

app :: Application
app = serve (Proxy :: Proxy API) server

-------- HANDLERS --------

issueHandler :: Handler [Issue]
issueHandler = fetchIssues return

queueHandler :: Handler H.Html
queueHandler = fetchIssues (return . renderHtml)

-------- FETCHING FUCNCTIONS --------

fetchIssues :: ( [Issue] -> Handler a ) -> Handler a
fetchIssues f = do
  issuesResult <- liftIO getGitlabEntpoint
  case issuesResult of
    Left err     -> throwError err500 { errBody = L8.pack err }
    Right issues -> f issues

-- Generate the client function
getGitLabIssues :: Int -> Maybe String -> Maybe String -> Maybe String -> ClientM [Issue]
getGitLabIssues = client (Proxy :: Proxy GitLabAPI)

-- Call the client function
getGitlabEntpoint :: IO (Either String [Issue])
getGitlabEntpoint = do
  manager <- newManager tlsManagerSettings 
  let baseUrl   = BaseUrl Https "git.gvk.idi.ntnu.no" 443 ""
      projectId = 5881
      labels    = Just "Announcement"
      state     = Just "opened"
      token     = Just gitlabToken
  res <- runClientM (getGitLabIssues projectId labels state token) (mkClientEnv manager baseUrl)
  return $ either (Left . show) Right res

-------- HTML FORMATTING FUNCTIONS --------

renderHtml :: [Issue] -> H.Html
renderHtml issues = H.docTypeHtml $ do
    H.head $ H.title "Issue Queue"
    H.body $ mapM_ issueToDiv issues
    H.link H.! A.rel "stylesheet" H.! A.href "/static/style.css"

issueToDiv :: Issue -> H.Html
issueToDiv Issue { web_url = issueUrl, title = t, project_id = i, state = s, author = Author {username = uname} } = 
  H.div $ do
    H.h3 $ H.toHtml t
    H.p $ H.toHtml $ "ID: " ++ show i
    H.p $ H.toHtml $ "Author: " ++ uname
    H.p $ H.toHtml $ "State: " ++ s
    H.a H.! A.href (H.toValue issueUrl) $ "View Issue"

