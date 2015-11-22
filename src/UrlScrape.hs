{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module UrlScrape where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LBC

import           Control.Exception          as X

import           Monad
import           StateMod
import           TH
import           Types


urlPattern :: ByteString
urlPattern = LB.init [litFile|src/urlPattern.txt|]

urlScraper :: Pipe (URI, ByteString) URI IO ()
urlScraper = forever $ do
    (base, content) <- await
    let ms = getAllTextMatches (content =~ urlPattern) :: [ByteString]
    forM_ ms $ \ub -> do
        let muri = parseURIReference $ LBC.unpack ub
        case muri of
            Nothing -> return () -- Not a valid URI
            Just u  -> do
                -- liftIO $ print $ uriPath uri
                let nu = nonStrictRelativeTo u base
                yield nu
{-
getUrls :: Pipe (Request, ByteString) ByteString Spider ()
getUrls = forever $ do
    (r, s) <- await
    let ms = getAllTextMatches (s =~ urlPattern) :: [ByteString]
    forM_ ms $ \u -> do
        let muri = parseURIReference . LBC.unpack $ u
        case muri of
            Nothing -> return ()
            Just uri -> do
                -- liftIO $ print $ uriPath uri
                mr <- liftIO $ (Just <$> setUriRelative r uri) `X.catch` exceptionHandler
                case mr of
                    Nothing -> return ()
                    Just req -> do
                        b <- isVisited req
                        when (not b) $ addRequest req
    yield s
-}
exceptionHandler :: HttpException -> IO (Maybe Request)
exceptionHandler e = return Nothing

