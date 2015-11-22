{-# LANGUAGE OverloadedStrings #-}
module Fetch where

import           Data.ByteString      (isPrefixOf)
import           Data.ByteString.Lazy (ByteString)

import           Control.Exception    as X

import           Monad
import           State
import           Types
import           Utils

fetcher :: Pipe URI (URI, ByteString) Spider ()
fetcher = visitedFilter >-> tee visitedMarker >-> prefetcher >-> contentFetcher


toRequest :: URI -> Maybe Request
toRequest uri = parseUrl $ show uri



isVisited :: URI -> Proxy a' a b' b Spider Bool
isVisited uri = do
    tvis <- use visited
    visSet <- liftIO $ atomically $ readTVar tvis
    return $ member uri visSet

markVisited :: URI -> Proxy a' a b' b Spider ()
markVisited uri = do
    tvis <- use visited
    liftIO $ atomically $ do
        visSet <- readTVar tvis
        let newSet = insert uri visSet
        writeTVar tvis newSet

visitedFilter :: Pipe URI URI Spider ()
visitedFilter = forever $ do
    uri <- await
    visited <- isVisited uri
    if visited
    then return ()
    else yield uri

prefetcher :: Pipe URI URI Spider ()
prefetcher = requestBuilder >-> prefetchRequester >-> statusCodeFilter >-> headerFilter >-> fstPicker

visitedMarker :: Consumer URI Spider ()
visitedMarker = forever $ do
    uri <- await
    liftIO $ appendFile "/tmp/url.txt" $ (++ "\n") $ show uri
    markVisited uri

requestBuilder :: Pipe URI (URI, Request) Spider ()
requestBuilder = forever $ do
    uri <- await
    case toRequest uri of
        Nothing -> return ()
        Just req -> yield (uri, req)

prefetchRequester :: Pipe (URI, Request) (URI, Response ()) Spider ()
prefetchRequester = forever $ do
    (uri, req) <- await
    man <- use manager
    mresp <- liftIO $ (Just <$> httpNoBody req man) `X.catch` statusExceptionHandler
    case mresp of
        Nothing -> return ()
        Just resp -> yield (uri, resp)

statusCodeFilter :: Pipe (URI, Response a) (URI, Response a) Spider ()
statusCodeFilter = forever $ do
    (uri, resp) <- await
    case statusCode $ responseStatus resp of
        200 -> yield (uri, resp)
        _ -> return ()

headerFilter :: Pipe (URI, Response a) (URI, Response a) Spider ()
headerFilter = forever $ do
    (uri, resp) <- await
    case lookup hContentType $ responseHeaders resp of
        Nothing -> return () -- play it safe
        Just str -> do
            if "text/" `isPrefixOf` str
            then yield (uri, resp)
            else return ()


contentFetcher :: Pipe URI (URI, ByteString) Spider ()
contentFetcher = requestBuilder >-> fetchRequester >-> statusCodeFilter >-> headerFilter >-> contentExtractor

contentExtractor :: Pipe (URI, Response a) (URI, a) Spider ()
contentExtractor = forever $ do
    (uri, resp) <- await
    yield (uri, responseBody resp)

fetchRequester :: Pipe (URI, Request) (URI, Response ByteString) Spider ()
fetchRequester = forever $ do
    (uri, req) <- await
    man <- use manager
    mresp <- liftIO $ (Just <$> httpLbs req man) `X.catch` statusExceptionHandler
    case mresp of
        Nothing -> return ()
        Just resp -> yield (uri, resp)




statusExceptionHandler :: HttpException -> IO (Maybe (Response a))
statusExceptionHandler e = return Nothing -- Ignore all errors
