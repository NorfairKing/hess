{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Fetch
import           MailScrape
import           Monad
import           Parser
import           Store
import           Types
import           UrlScrape

import           Data.ByteString.Lazy (ByteString)

import qualified Data.Set             as S

-- We'll have to change fromInput and fromOutput to not just stop when they're done.
spider :: URI -> HESS ()
spider uri = do
    man <- liftIO $ newManager tlsManagerSettings

    let startVisited = S.empty
    visitedSet <- liftIO $ newTVarIO startVisited

    let startState = CState {
              _manager = man
            , _visited = visitedSet
        }

    (uriOut, uriIn)                 <- liftIO $ spawn unbounded
    (contentOut, contentIn)         <- liftIO $ spawn $ bounded 100
    (urlScraperOut, urlScraperIn)   <- liftIO $ spawn $ bounded 100
    (mailScraperOut, mailScraperIn) <- liftIO $ spawn $ bounded 100

    let duper :: Consumer (URI, ByteString) HESS ()
        duper = forever $ do
            (uri, content) <- await
            runEffect $ yield (uri, content) >-> toOutput urlScraperOut
            runEffect $ yield content >-> toOutput mailScraperOut

    runEffect $ yield uri >-> toOutput uriOut -- Start the process off

    let n = 4

    config <- ask

    -- Todo make helper functions that help here.
    as <- liftIO
        $ forM [1..n]
        $ \i -> async $ void
            $ (flip runReaderT) config
            $ (flip evalStateT) startState
                $ do runEffect $ fromInput uriIn >-> fetcher i >-> toOutput contentOut
                     liftIO performGC

    s <- liftIO
        $ async $ void $ (flip runReaderT) config
                       $ do runEffect $ fromInput contentIn >-> duper
                            liftIO performGC

    u <- liftIO
        $ async $ void $ (flip runReaderT) config
                       $ do runEffect $ fromInput urlScraperIn >-> uriScraper >-> toOutput uriOut
                            liftIO performGC
    m <- liftIO
        $ async $ void $ (flip runReaderT) config
                       $ do runEffect $ fromInput mailScraperIn >-> mailScraper >-> store
                            liftIO performGC

    liftIO $ mapM_ wait (m:s:u:as)

main :: IO ()
main = do
    (seed, args) <- parseArgs
    case parseURI seed of
        Nothing -> error "Invalid seed URI"
        Just uri -> runReaderT (spider uri) args

