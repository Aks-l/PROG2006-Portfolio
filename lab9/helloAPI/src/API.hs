{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Servant
import Types

type API = 
       Get '[PlainText] String
  :<|> "hello" :> Get '[PlainText] String
  :<|> "greet" :> Capture "name" String :> Get '[JSON] GreetStruct
  :<|> "greetme" :> ReqBody '[JSON] GreetMeStruct :> Post '[JSON] GreetMeResponse
