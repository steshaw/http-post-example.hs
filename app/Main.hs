{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Lens
import Data.Aeson
import Data.Foldable
import Network.Wreq

postTest s = do
  r <- post "http://httpbin.org/post" (partText "q" s)
  let Just obj = decode (r ^. responseBody)
  pure (obj :: Object)

posts = map postTest ["Hello", "There", "Foo", "Bar"]

main = do
  a <- runConcurrently $ asum (map Concurrently posts)
  print a
