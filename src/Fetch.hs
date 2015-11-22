{-# LANGUAGE OverloadedStrings #-}
module Fetch where

import           Data.ByteString      (isPrefixOf)
import           Data.ByteString.Lazy (ByteString)

import           Control.Exception    as X

import           Monad
import           State
import           StateMod
import           Types

crawlProducer :: Producer (Request, ByteString) Spider ()
crawlProducer = crawlProducer' 0

crawlProducer' :: Int -> Producer (Request, ByteString) Spider ()
crawlProducer' tries = do
    mp <- getNext
    case mp of
        Nothing -> if tries < 10 -- TODO config-ify
                   then do
                        liftIO $ threadDelay 1000000 -- TODO config-ify
                        crawlProducer' $ tries + 1
                   else return ()
        Just req -> do
            markVisited req
            man <- readStates _manager


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
                                        yield (req, body)
                                    else return ()

                        _   -> return ()

            crawlProducer' 0


statusExceptionHandler :: HttpException -> IO (Maybe (Response ByteString))
statusExceptionHandler e = return Nothing
