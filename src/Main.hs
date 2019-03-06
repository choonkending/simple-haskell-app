{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai(Application, responseLBS, queryString)
import Network.Wai.Handler.Warp(run)
import Network.HTTP.Types.Status(status200)
import Control.Monad(join)
import Data.Maybe(fromMaybe)
import Data.ByteString.Lazy(fromStrict)

app :: Application
app req respond = respond (responseLBS status200 [] body) where
  query = queryString req
  message = join (lookup "message" query)
  body = fromStrict (fromMaybe "empty" message)

main :: IO ()
main = do
  run 8080 app
