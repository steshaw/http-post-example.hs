{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Conceit
import Data.Maybe (maybe)
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key)
import Data.Foldable
import Network.Wreq

echo :: Integer -> IO (Either Value String)
echo i =
  if i == 5 then pure $ Right "5 fails"
  else do
    print i
    r <- post "http://httpbin.org/post" (toJSON i)
    pure $ maybe (Right "Malformed response") Left (r ^? responseBody . key "json")

main = do
  first <- mapConceit echo [1..10]
  print first
