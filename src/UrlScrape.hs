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
import           Store
import           TH
import           Types

urlPattern :: ByteString
urlPattern = LB.init [litFile|src/urlPattern.txt|]

uriScraper :: Pipe (URI, ByteString) URI IO ()
uriScraper = uriMatcher >-> uriParser -- >-> tee uriPrinter

uriMatcher :: Pipe (URI, ByteString) (URI, String) IO ()
uriMatcher = forever $ do
    (base, content) <- await
    let ms = getAllTextMatches (content =~ urlPattern) :: [ByteString]
    forM_ ms $ \ub -> liftIO (print ub) >> yield (base, LBC.unpack ub)

uriParser :: Pipe (URI, String) URI IO ()
uriParser = forever $ do
    (base, new) <- await
    yieldMaybe $ tryRelative base new
    yieldMaybe $ tryAbsolute new
    yieldMaybe $ tryAbsoluteWithScheme "http" new
    -- yieldMaybe $ tryAbsoluteWithScheme "https" new

yieldMaybe :: Maybe a -> Pipe b a IO ()
yieldMaybe Nothing = return ()
yieldMaybe (Just a) = yield a

tryRelative :: URI -> String -> Maybe URI
tryRelative uri s = do
    rel <- parseRelativeReference s
    return $ relativeTo uri rel

tryAbsolute :: String -> Maybe URI
tryAbsolute s = parseAbsoluteURI s

tryAbsoluteWithScheme :: String -> String -> Maybe URI
tryAbsoluteWithScheme scheme s = parseAbsoluteURI $ scheme ++ "://" ++ s

{-
uriPrinter :: Consumer URI IO ()
uriPrinter = forever $ do
    uri <- await
    liftIO $ print uri
-}
