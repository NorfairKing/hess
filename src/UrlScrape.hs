{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module UrlScrape where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LBC


import           Monad
import           StateMod
import           TH
import           Types


urlPattern :: ByteString
urlPattern = LB.init [litFile|src/urlPattern.txt|]

getUrls :: Pipe (Request, ByteString) ByteString Spider ()
getUrls = forever $ do
    (r, s) <- await
    let ms = getAllTextMatches (s =~ urlPattern) :: [ByteString]
    forM_ ms $ \u -> do
        let muri = parseURIReference . LBC.unpack $ u
        case muri of
            Nothing -> return ()
            Just uri -> do
                req <- liftIO $ setUriRelative r uri
                addRequest req
    yield s

