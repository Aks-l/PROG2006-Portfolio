{-# LANGUAGE TypeOperators #-}
module Server (app) where

import Network.Wai (Application)
import Servant
import API
import Handlers

server :: Server API
server = rootHandler
    :<|> helloHandler
    :<|> greetHandler
    :<|> greetMeHandler

app :: Application
app = serve (Proxy :: Proxy API) server
