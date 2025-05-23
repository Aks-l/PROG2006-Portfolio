# Lab 9 - Api for overlooking active issues in the PROG2006 Gitlab

## Endpoints
### /queue

See all issues in order of creation data.
Presented with HTML and CSS

### /issues

See all issues as raw JSON data

## GitLab integration

### Token
 To use, create or use an existing Gitlab token and insert in `Token.hs` before running.

### Data selecting
Path to Gitlab, the project, and queries are hard coded, but easy to change if other projects or lables are desired; see the variables declared below:

```hs
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
```

### Data fetching
While the data selecting point sbove ddecides which data should be found, this section actually forms the search query to send the request.
he entire path, including base path and query parameters are defined here, including the specification of a GET-request. 

```hs
type GitLabAPI =
       "api" :> "v4" :> "projects" :> Capture "projectId" Int
             :> "issues"
             :> QueryParam "labels" String
             :> QueryParam "state" String
             :> Header "PRIVATE-TOKEN" String
             :> Get '[JSON] [Issue]
```

### Data serving
The data is at all times stored in a datatype that is easily converted into a JSON-object.

```hs
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
```

This data is then either sreved directly as JSON or formatted based on which endpoint is accessed.

The formatting happens by creating an HTML object per issue, and is then serving a static CSS file to the endpoint as well.

```hs
renderHtml :: [Issue] -> H.Html
renderHtml issues = H.docTypeHtml $ do
    H.head $ do
        H.title "Prog2006 Issue Queue"
        H.link H.! A.rel "stylesheet" H.! A.href "/static/style.css"
    H.body $ do
      H.h1 H.! A.class_ "header" $ "Prog2006 Issue Queue"
      mconcat $ zipWith issueToDiv [1..] (sortOn created_at issues)
```

## Features and learning aspects

This task has made me use several libraries and features of Haskell I have not used before. 
The entire API-defenition consisting of operators like `:>` and `:<|>`, as well as JSON parsing operators like `.:` and `.:?` really shows how Haskell performs eerything functionally and in a completely different way than other languages.
It is also an interesting feature where the defenition of the endpoint with its types and paths are connected to the server only through the order the paths and functions are served. This is definitley different from all other languages i have used, but once understood, it feels really nice as it is very compact when working. It is however a bit of a hassle to edit, as one change often requires another place to be changed accordingly.