{-# LANGUAGE DuplicateRecordFields #-}
module Handlers where

import Servant
import Types

rootHandler :: Handler String 
rootHandler = throwError err404

helloHandler :: Handler String
helloHandler = return "Hello World!"

greetHandler :: String -> Handler GreetStruct
greetHandler name = return $ GreetStruct { msg = "Hello", name = name }

greetMeHandler :: GreetMeStruct -> Handler GreetMeResponse
greetMeHandler (GreetMeStruct input name) = return $ GreetMeResponse { msg = input ++ " " ++ name }
