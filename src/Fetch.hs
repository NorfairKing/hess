{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Control.Monad
import Data.ByteString (isPrefixOf)
import qualified Data.ByteString as SB (ByteString)

import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Exception as X

import Monad
import Types
import Utils

{-# ANN module ("HLint: ignore Use hierarchical imports" :: String)
        #-}

newtype CrawlerState = CState
    { manager :: Manager
    }

type Fetcher = StateT CrawlerState HESS

fetcher :: Int -> Pipe URI (URI, ByteString) Fetcher ()
fetcher nr = do
    l <- asks fetchers_status_logging
    let status =
            if l
                then tee (statusLight nr)
                else cat
    status >-> prefetcher >-> contentFetcher

statusLight :: Int -> Consumer URI Fetcher ()
statusLight nr = go 0
  where
    go :: POSIXTime -> Consumer URI Fetcher ()
    go prev =
        forever $ do
            f <- asks fetchers_status_logging_file
            void await
            now <- liftIO getPOSIXTime
            if prev + 1 < now
                then do
                    liftIO $
                        appendFile f $
                        replicate (2 * (nr - 1)) ' ' ++ show nr ++ "\n"
                    go now
                else go prev

toRequest :: URI -> Maybe Request
toRequest = parseUrlThrow . show

prefetcher :: Pipe URI URI Fetcher ()
prefetcher =
    requestBuilder >-> prefetchRequester >-> statusCodeFilter >-> headerFilter >->
    fstPicker

requestBuilder :: Pipe URI (URI, Request) Fetcher ()
requestBuilder =
    forever $ do
        uri <- await
        case toRequest uri of
            Nothing -> return ()
            Just req -> yield (uri, req)

prefetchRequester :: Pipe (URI, Request) (URI, Response ()) Fetcher ()
prefetchRequester =
    forever $ do
        (uri, req) <- await
        man <- gets manager
        mresp <-
            liftIO $
            (Just <$> httpNoBody req man) `X.catch` statusExceptionHandler
        case mresp of
            Nothing -> return ()
            Just resp -> yield (uri, resp)

statusCodeFilter :: Pipe (URI, Response a) (URI, Response a) Fetcher ()
statusCodeFilter =
    forever $ do
        (uri, resp) <- await
        case statusCode $ responseStatus resp of
            200 -> yield (uri, resp)
            _ -> return ()

headerFilter :: Pipe (URI, Response a) (URI, Response a) Fetcher ()
headerFilter =
    forever $ do
        (uri, resp) <- await
        case lookup hContentType $ responseHeaders resp of
            Nothing -> return () -- play it safe
            Just str -> when (good str) $ yield (uri, resp)
  where
    good :: SB.ByteString -> Bool
    good str
        | "text/javascript" `isPrefixOf` str = False
    good str
        | "text/x" `isPrefixOf` str = False -- General source code files
    good str
        | "text/" `isPrefixOf` str = True
    good _ = False

contentFetcher :: Pipe URI (URI, ByteString) Fetcher ()
contentFetcher =
    requestBuilder >-> fetchRequester >-> statusCodeFilter >-> headerFilter >->
    contentExtractor

contentExtractor :: Pipe (URI, Response a) (URI, a) Fetcher ()
contentExtractor =
    forever $ do
        (uri, resp) <- await
        yield (uri, responseBody resp)

fetchRequester :: Pipe (URI, Request) (URI, Response ByteString) Fetcher ()
fetchRequester =
    forever $ do
        (uri, req) <- await
        man <- gets manager
        mresp <-
            liftIO $ (Just <$> httpLbs req man) `X.catch` statusExceptionHandler
        case mresp of
            Nothing -> return ()
            Just resp -> yield (uri, resp)

statusExceptionHandler :: HttpException -> IO (Maybe (Response a))
statusExceptionHandler _ = return Nothing -- Ignore all errors
