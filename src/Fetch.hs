{-# LANGUAGE OverloadedStrings #-}
module Fetch where

import           Data.ByteString       (isPrefixOf)
import qualified Data.ByteString       as SB (ByteString)

import           Data.ByteString.Lazy  (ByteString)
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           Control.Exception     as X

import           Monad
import           Types
import           Utils

data CrawlerState = CState {
          _visited :: TVar (Set URI)
        , _manager :: Manager
    }
type Fetcher = StateT CrawlerState HESS

makeLenses ''CrawlerState

fetcher :: Int -> Pipe URI (URI, ByteString) Fetcher ()
fetcher nr = statusLight nr >->
    visitedFilter >->
    tee visitedMarker >->
    prefetcher >->
    contentFetcher

statusLight :: Int -> Pipe URI URI Fetcher ()
statusLight nr = go $ fromInteger 0
  where
    go prev = forever $ do
        u <- await
        now <- liftIO getPOSIXTime
        if prev + 1 < now
        then do
            liftIO $ appendFile "/tmp/worker.txt" $ show nr ++ "\n"
            yield u
            go now
        else do
            yield u
            go prev


toRequest :: URI -> Maybe Request
toRequest uri = parseUrl $ show uri


isVisited :: URI -> Proxy a' a b' b Fetcher Bool
isVisited uri = do
    tvis <- use visited
    visSet <- liftIO $ atomically $ readTVar tvis
    return $ member uri visSet

markVisited :: URI -> Proxy a' a b' b Fetcher ()
markVisited uri = do
    tvis <- use visited
    liftIO $ atomically $ do
        visSet <- readTVar tvis
        let newSet = insert uri visSet
        writeTVar tvis newSet

visitedFilter :: Pipe URI URI Fetcher ()
visitedFilter = forever $ do
    uri <- await
    visited <- isVisited uri
    if visited
    then return ()
    else yield uri

prefetcher :: Pipe URI URI Fetcher ()
prefetcher = requestBuilder >-> prefetchRequester >-> statusCodeFilter >-> headerFilter >-> fstPicker

visitedMarker :: Consumer URI Fetcher ()
visitedMarker = forever $ do
    uri <- await
    liftIO $ appendFile "/tmp/url.txt" $ (++ "\n") $ show uri
    markVisited uri

requestBuilder :: Pipe URI (URI, Request) Fetcher ()
requestBuilder = forever $ do
    uri <- await
    case toRequest uri of
        Nothing -> return ()
        Just req -> yield (uri, req)

prefetchRequester :: Pipe (URI, Request) (URI, Response ()) Fetcher ()
prefetchRequester = forever $ do
    (uri, req) <- await
    man <- use manager
    mresp <- liftIO $ (Just <$> httpNoBody req man) `X.catch` statusExceptionHandler
    case mresp of
        Nothing -> return ()
        Just resp -> yield (uri, resp)

statusCodeFilter :: Pipe (URI, Response a) (URI, Response a) Fetcher ()
statusCodeFilter = forever $ do
    (uri, resp) <- await
    case statusCode $ responseStatus resp of
        200 -> yield (uri, resp)
        _ -> return ()

headerFilter :: Pipe (URI, Response a) (URI, Response a) Fetcher ()
headerFilter = forever $ do
    (uri, resp) <- await
    case lookup hContentType $ responseHeaders resp of
        Nothing -> return () -- play it safe
        Just str -> do
            if good str
            then yield (uri, resp)
            else return ()
  where
    good :: SB.ByteString -> Bool
    good str | "text/javascript" `isPrefixOf` str   = False
    good str | "text/x" `isPrefixOf` str            = False -- General source code files
    good str | "text/" `isPrefixOf` str             = True
    good _                                          = False

contentFetcher :: Pipe URI (URI, ByteString) Fetcher ()
contentFetcher = requestBuilder >-> fetchRequester >-> statusCodeFilter >-> headerFilter >-> contentExtractor

contentExtractor :: Pipe (URI, Response a) (URI, a) Fetcher ()
contentExtractor = forever $ do
    (uri, resp) <- await
    yield (uri, responseBody resp)

fetchRequester :: Pipe (URI, Request) (URI, Response ByteString) Fetcher ()
fetchRequester = forever $ do
    (uri, req) <- await
    man <- use manager
    mresp <- liftIO $ (Just <$> httpLbs req man) `X.catch` statusExceptionHandler
    case mresp of
        Nothing -> return ()
        Just resp -> yield (uri, resp)




statusExceptionHandler :: HttpException -> IO (Maybe (Response a))
statusExceptionHandler e = return Nothing -- Ignore all errors


