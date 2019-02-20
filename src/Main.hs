{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai(Application, responseLBS)
import Network.Wai.Handler.Warp(run)
import Network.HTTP.Types.Status(status200)

app :: Application
app req respond = respond (responseLBS status200 [] "Hello World")

main :: IO ()
main = do
  run 8080 app
