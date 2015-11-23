{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module UrlScrape where

import qualified Data.ByteString            as SB
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LBC

import           Control.Exception          as X

import           TH
import           Types
import           Utils

urlPattern :: ByteString
urlPattern = LB.init [litFile|src/urlPattern.txt|]

scriptPattern :: String
scriptPattern = init [litFile|src/scriptPattern.txt|]

uriScraper :: Pipe (URI, ByteString) URI IO ()
uriScraper =
    -- contentCleaner >->
    uriMatcher >->
    uriParser >->
    uriCleaner

contentCleaner :: Pipe (URI, ByteString) (URI, ByteString) IO ()
contentCleaner = forever $ do
    (uri, content) <- await
    clean uri content
  where
    clean uri content = do
        let (b1, p1, a1) = content =~ scriptPattern :: (ByteString, ByteString, ByteString)
        yield (uri, b1)
        if LB.null a1
        then return ()
        else clean uri a1


uriMatcher :: Pipe (URI, ByteString) (URI, String) IO ()
uriMatcher = forever $ do
    (base, content) <- await
    let ms = getAllTextMatches (content =~ urlPattern) :: [ByteString]
    forM_ ms $ \ub -> yield (base, LBC.unpack ub)

uriParser :: Pipe (URI, String) URI IO ()
uriParser = forever $ do
    (base, new) <- await
    yieldMaybe $ tryRelative base new
    yieldMaybe $ tryAbsolute new
    yieldMaybe $ tryAbsoluteWithScheme "http" new
    -- yieldMaybe $ tryAbsoluteWithScheme "https" new

tryRelative :: URI -> String -> Maybe URI
tryRelative uri s = do
    rel <- parseRelativeReference s
    return $ relativeTo uri rel

tryAbsolute :: String -> Maybe URI
tryAbsolute s = parseAbsoluteURI s

tryAbsoluteWithScheme :: String -> String -> Maybe URI
tryAbsoluteWithScheme scheme s = parseAbsoluteURI $ scheme ++ "://" ++ s

uriCleaner :: Pipe URI URI IO ()
uriCleaner = forever $ do
    uri <- await
    yield $ uri { uriQuery = [], uriFragment = [] }
