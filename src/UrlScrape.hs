{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module UrlScrape where

import qualified Data.ByteString            as SB
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LBC

import           Text.HTML.TagSoup

import           Control.Exception          as X

import qualified Pipes.Prelude              as P (print)

import           Monad
import           TH
import           Types
import           Utils

urlPattern :: ByteString
urlPattern = LB.init [litFile|src/urlPattern.txt|]

scriptPattern :: String
scriptPattern = init [litFile|src/scriptPattern.txt|]

uriScraper :: Pipe (URI, ByteString) URI HESS ()
uriScraper =
    parser >->
    aFilter >->
    hrefFilter >->
    unpacker >->
    uriParser >->
    uriCleaner

parser :: Pipe (URI, ByteString) (URI, Tag ByteString) HESS ()
parser = forever $ do
    (uri, content) <- await
    forM_ (parseTags content) $ \t -> yield (uri, t)

aFilter :: Pipe (URI, Tag ByteString) (URI, Attribute ByteString) HESS ()
aFilter = forever $ do
    (u, t) <- await
    case t of
        TagOpen "a" atts -> forM_ atts $ \a -> yield (u, a)
        _ -> return ()

hrefFilter :: Pipe (URI, Attribute ByteString) (URI, ByteString) HESS ()
hrefFilter = forever $ do
    (u, a) <- await
    case a of
        ("href", link) -> yield (u, link)
        _ -> return ()

unpacker :: Pipe (URI, ByteString) (URI, String) HESS ()
unpacker = forever $ do
    (u, l) <- await
    yield (u, LBC.unpack l)

uriParser :: Pipe (URI, String) URI HESS ()
uriParser = forever $ do
    (base, new) <- await
    if isURI new
    then yieldMaybe $ tryAbsolute new
    else if isRelativeReference new
        then yieldMaybe $ tryRelative base new
        else yieldMaybe $ tryAbsoluteWithScheme "http" new

tryRelative :: URI -> String -> Maybe URI
tryRelative uri s = do
    rel <- parseRelativeReference s
    return $ relativeTo uri rel

tryAbsolute :: String -> Maybe URI
tryAbsolute s = parseAbsoluteURI s

tryAbsoluteWithScheme :: String -> String -> Maybe URI
tryAbsoluteWithScheme scheme s = parseAbsoluteURI $ scheme ++ "://" ++ s

uriCleaner :: Pipe URI URI HESS ()
uriCleaner = forever $ do
    uri <- await
    let clean = uri { uriQuery = [], uriFragment = [] }
    yield clean
