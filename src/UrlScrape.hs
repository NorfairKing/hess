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
                -- parseRelativeReference and parseAbsoluteURI seperately!
                let nu = relativeTo u base
                liftIO $ print nu
                yield nu
