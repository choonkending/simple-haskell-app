{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai(Application, Middleware, Request, ifRequest, requestMethod, pathInfo, responseLBS, queryString)
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
app = z notFound

notFound :: Application
notFound _ respond =  respond (responseLBS status404 [] "Not Found")

handleNameRequestMiddleware :: Middleware
handleNameRequestMiddleware _ = handleNameRequest

z :: Middleware
z = ifRequest3 isNameRequest handleNameRequest

ifRequest2 :: (Request -> Bool) -> Middleware -> Middleware -> Middleware
ifRequest2 pred success fail app req = if pred req
  then success app req
  else fail app req

ifRequest3 :: (Request -> Bool) -> Application -> Middleware
ifRequest3 pred app1 app2 req = if pred req
  then app1 req
  else app2 req

isNameRequest :: Request -> Bool
isNameRequest req =
  case (requestMethod req, pathInfo req) of
    (method, ["names"]) | method == methodPost -> True
    _ -> False

handleNameRequestMiddleware2 :: Middleware
handleNameRequestMiddleware2 app1 = app2 where
  app2 req respond = if isNameRequest req
    then handleNameRequest req respond
    else app1 req respond
  isNameRequest req =
    case (requestMethod req, pathInfo req) of
      (method, ["names"]) | method == methodPost -> True
      _ -> False

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
