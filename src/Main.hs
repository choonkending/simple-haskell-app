{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai(Application, responseLBS, queryString)
import Network.Wai.Handler.Warp(run)
import Network.Wai.Parse(parseRequestBodyEx, defaultParseRequestBodyOptions, Param, FileInfo)
import Network.HTTP.Types.Status(status200, status400)
import Control.Monad(join)
import Data.Maybe(fromMaybe)
import Data.ByteString.Lazy(fromStrict)
import Data.ByteString(ByteString)
import Data.Monoid((<>))

app :: Application
app req respond = do
  (params, _) <- parseRequestBodyEx defaultParseRequestBodyOptions backend req
  let name = combineNames params
  case name of
    Just body -> respond (responseLBS status200 [] (fromStrict body))
    Nothing -> respond (responseLBS status400 [] "no")

backend :: ByteString -> FileInfo () -> IO ByteString -> IO a
backend _ _ _ = error "dont want file upload"

combineNames :: [Param] -> Maybe ByteString
combineNames params = do
  firstName <- lookup "firstName" params
  lastName <- lookup "lastName" params
  pure (firstName <> " " <> lastName)

main :: IO ()
main = do
  run 8080 app
