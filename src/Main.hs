{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai(Application, Middleware, Request, requestHeaders, Response, ResponseReceived, requestMethod, pathInfo, responseLBS, queryString, mapResponseHeaders)
import Network.Wai.Handler.Warp(run)
import Network.Wai.Parse(parseRequestBodyEx, defaultParseRequestBodyOptions, Param, FileInfo)
import Network.HTTP.Types.Header(ResponseHeaders)
import Network.HTTP.Types.Status(status200, status400, status404)
import Network.HTTP.Types.Method(methodPost, methodGet)
import Control.Monad(join)
import Data.Maybe(fromMaybe, listToMaybe)
import Data.ByteString.Lazy(fromStrict)
import Data.ByteString(ByteString)
import Data.Monoid((<>))
import Debug.Trace(trace)
import Data.UUID(UUID)
import qualified Data.UUID.V4 as UUID(nextRandom)
import qualified Data.UUID as UUID(toASCIIBytes)

app :: Application
app = applicationMiddleware notFound

notFound :: Application
notFound _ respond =  respond (responseLBS status404 [] "Not Found")

success :: Application
success _ respond =  respond (responseLBS status200 [] "OK")

successMiddleware :: Middleware
successMiddleware = ifRequest isSuccessRequest success where
  isSuccessRequest req =
    case (requestMethod req, pathInfo req) of
      (method, ["healthCheck"]) | method == methodGet -> True
      _ -> False

handleNameRequestMiddleware :: Middleware
handleNameRequestMiddleware = ifRequest isNameRequest handleNameRequest

-- if has request id in header, pass it to x-transaction-id in header
-- otherwise, generate uuid and pass it to x-transaction-id in header
transactionIdMiddleware :: Middleware
transactionIdMiddleware app = appWithTransactionId where
  appWithTransactionId req respond = do
    let requestTransactionID = getTransactionId req
    transactionID <- maybe newTransactionID pure requestTransactionID
    app req (propagateTransactionID transactionID respond)

propagateTransactionID :: ByteString -> (Response -> IO ResponseReceived) -> Response -> IO ResponseReceived
propagateTransactionID transactionID respond response =
  respond (mapResponseHeaders (addResponseHeader transactionID) response)

getTransactionId :: Request -> Maybe ByteString
getTransactionId req = lookup "x-transaction-id" $ requestHeaders req

addResponseHeader :: ByteString -> ResponseHeaders -> ResponseHeaders
addResponseHeader transactionID = (transactionIDHeader :) where
  transactionIDHeader = ("x-transaction-id", transactionID)

newTransactionID :: IO ByteString
newTransactionID = UUID.toASCIIBytes <$> UUID.nextRandom

applicationMiddleware :: Middleware
applicationMiddleware = transactionIdMiddleware . successMiddleware . handleNameRequestMiddleware


-- ifRequest :: (Request -> Bool) -> Application -> Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- ifRequest :: (Request -> Bool) -> Application -> Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- ifRequest pred app1 app2 req respond = if pred req
--   then app1 req respond
--   else app2 req respond

ifRequest :: (Request -> Bool) -> Application -> Middleware
ifRequest pred app1 app2 req = if pred req
   then app1 req
   else app2 req

isNameRequest :: Request -> Bool
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
