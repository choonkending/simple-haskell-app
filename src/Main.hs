{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai(Application, Middleware, requestMethod, pathInfo, responseLBS, queryString)
import Network.Wai.Handler.Warp(run)
import Network.Wai.Parse(parseRequestBodyEx, defaultParseRequestBodyOptions, Param, FileInfo)
import Network.HTTP.Types.Status(status200, status400, status404)
import Network.HTTP.Types.Method(methodPost)
import Control.Monad(join)
import Data.Maybe(fromMaybe, listToMaybe)
import Data.ByteString.Lazy(fromStrict)
import Data.ByteString(ByteString)
import Data.Monoid((<>))
import Debug.Trace(trace)

app :: Application
app = handleNameRequestMiddleware notFound

notFound :: Application
notFound _ respond =  respond (responseLBS status404 [] "Not Found")

handleNameRequestMiddleware :: Middleware
handleNameRequestMiddleware app1 = app2 where
  app2 req respond = if isNameRequest req
    then handleNameRequest req respond
    else app1 req respond
  isNameRequest req =
    let path = listToMaybe (pathInfo req)
    in case path of
      Just pathName -> if requestMethod req == methodPost && pathName == "names"
        then True
        else False
      Nothing -> False

handleNameRequest :: Application
handleNameRequest req respond = do
  (params, _) <- parseRequestBodyEx defaultParseRequestBodyOptions backend req
  let name = combineNames params
  case name of
    Just body -> respond (responseLBS status200 [] (fromStrict body))
    Nothing -> respond (responseLBS status400 [] "no names")

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
