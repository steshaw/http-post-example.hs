{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent.Async
import Data.Aeson
import Data.Foldable
import Network.HTTP.Client

buildRequest :: String -> RequestBody -> IO Request
buildRequest url body = do
  nakedRequest <- parseRequest url
  return (nakedRequest { method = "POST", requestBody = body })

send :: RequestBody -> IO ()
send s = do
  manager <- newManager defaultManagerSettings
  request <- buildRequest "http://httpbin.org/post" s
  response <- httpLbs request manager
  let Just obj = decode (responseBody response)
  print (obj :: Object)

posts = [send "Hello",
         send "There",
         send "Foo",
         send "Bar"]

main = do
  a <- runConcurrently $ asum (map Concurrently posts)
  print $ a
