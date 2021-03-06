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
import           UrlProcess
import           UrlScrape

import           Data.ByteString.Lazy (ByteString)

import qualified Data.Set             as S

main :: IO ()
main = parseArgs >>= runReaderT spider

spider :: HESS ()
spider = do
    uri <- view seed_uri
    man <- liftIO $ newManager tlsManagerSettings

    let startVisited = S.empty
    onQueueSet <- liftIO $ newTVarIO startVisited

    let startCState = CState {
              _manager = man
        }

    let startPState = PState {
              _onQueue = onQueueSet
        }

    (uriOut, uriIn)                 <- liftIO $ spawn unbounded
    (contentOut, contentIn)         <- liftIO $ spawn $ bounded 100
    (urlScraperOut, urlScraperIn)   <- liftIO $ spawn $ bounded 100
    (mailScraperOut, mailScraperIn) <- liftIO $ spawn $ bounded 100
    (urlProcessOut, urlProcessIn)   <- liftIO $ spawn $ unbounded

    let duper :: Consumer (URI, ByteString) HESS ()
        duper = forever $ do
            (uri, content) <- await
            runEffect $ yield (uri, content) >-> toOutput urlScraperOut
            runEffect $ yield content >-> toOutput mailScraperOut

    runEffect $ yield uri >-> toOutput uriOut -- Start the process off

    n <- view nr_fetchers

    config <- ask

    as <- liftIO
        $ forM [1..n]
        $ \i -> async $ void
            $ (flip runReaderT) config
            $ (flip evalStateT) startCState
                $ do runEffect $ fromInput uriIn >-> fetcher i >-> toOutput contentOut
                     liftIO performGC

    s <- liftIO
        $ async $ void $ (flip runReaderT) config
                       $ do runEffect $ fromInput contentIn >-> duper
                            liftIO performGC

    u <- liftIO
        $ async $ void $ (flip runReaderT) config
                       $ do runEffect $ fromInput urlScraperIn >-> uriScraper >-> toOutput urlProcessOut
                            liftIO performGC
    m <- liftIO
        $ async $ void $ (flip runReaderT) config
                       $ do runEffect $ fromInput mailScraperIn >-> mailScraper >-> store
                            liftIO performGC
    p <- liftIO
        $ async $ void $ (flip runReaderT) config
                       $ (flip evalStateT) startPState
                       $ do runEffect $ fromInput urlProcessIn >-> urlProcess >-> toOutput uriOut

    liftIO $ mapM_ wait (m:s:u:p:as)



