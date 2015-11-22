{-# LANGUAGE OverloadedStrings #-}
module Fetch where

import           Data.ByteString      (isPrefixOf)
import           Data.ByteString.Lazy (ByteString)

import           Control.Exception    as X

import           Monad
import           State
import           StateMod
import           Types


fetcher :: Pipe URI (URI, ByteString) Spider ()
fetcher = forever $ do
    uri <- await
    visited <- isVisited uri
    if visited
    then return () -- Put this in another pipe entirely?
    else do
        markVisited uri
        liftIO $ appendFile "/tmp/url.txt" $ (++ "\n") $ show uri
        let mr = toRequest uri
        case mr of
            Nothing -> return () -- Was unable to build a request
            Just req -> do
                man <- use manager
                -- TODO first to head request to check content type, skip if not text.
                mr <- liftIO $ (Just <$> httpLbs req man) `X.catch` statusExceptionHandler
                case mr of
                    Nothing -> return ()
                    Just resp -> do
                        case statusCode $ responseStatus resp of
                            200 -> do
                                case lookup hContentType $ responseHeaders resp of
                                    Nothing -> return () -- Just to be sure.
                                    Just str -> do
                                        if "text/" `isPrefixOf` str
                                        then do
                                            let body = responseBody resp
                                            yield (uri, body)
                                        else return ()
                            _   -> return ()


toRequest :: URI -> Maybe Request
toRequest uri = parseUrl $ show uri

statusExceptionHandler :: HttpException -> IO (Maybe (Response ByteString))
statusExceptionHandler e = return Nothing -- Ignore all errors


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
