{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Fetch
import           MailScrape
import           Parser
import           Store
import           Types
import           UrlScrape

import           Data.ByteString.Lazy (ByteString)

import qualified Data.Set             as S

-- We'll have to change fromInput and fromOutput to not just stop when they're done.
spider :: URI -> IO ()
spider uri = do
    man <- newManager tlsManagerSettings

    let startVisited = S.empty
    visitedSet <- newTVarIO startVisited

    let startState = CState {
              _manager = man
            , _visited = visitedSet
        }

    (uriOut, uriIn) <- spawn unbounded
    (contentOut, contentIn) <- spawn $ bounded 100
    (urlScraperOut, urlScraperIn) <- spawn $ bounded 100
    (mailScraperOut, mailScraperIn) <- spawn $ bounded 100

    let duper :: Consumer (URI, ByteString) IO ()
        duper = forever $ do
            (uri, content) <- await
            runEffect $ yield (uri, content) >-> toOutput urlScraperOut
            runEffect $ yield content >-> toOutput mailScraperOut

    runEffect $ yield uri >-> toOutput uriOut -- Start the process off

    let n = 4

    -- Todo make helper functions that help here.
    as <- forM [1..n] $ \i ->
        async $ void
            $ (flip evalStateT) startState
                $ do runEffect $ fromInput uriIn >-> fetcher i >-> toOutput contentOut
                     liftIO performGC

    s <- async $ void $ do runEffect $ fromInput contentIn >-> duper
                           performGC

    u <- async $ void $ do runEffect $ fromInput urlScraperIn >-> uriScraper >-> toOutput uriOut
                           performGC
    m <- async $ void $ do runEffect $ fromInput mailScraperIn >-> mailScraper >-> store
                           performGC

    mapM_ wait (m:s:u:as)

main :: IO ()
main = do
    (seed, args) <- parseArgs
    case parseURI seed of
        Nothing -> error "Invalid seed URI"
        Just uri -> spider uri

