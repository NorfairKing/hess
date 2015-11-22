{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Fetch
import           MailScrape
import           Parser
import           State
import           Store
import           Types
import           UrlScrape

import qualified Data.Set   as S

spider :: String -> IO ()
spider url = do
    r <- liftIO $ parseUrl url
    let req = r { checkStatus = \s rh cj -> Nothing } -- Don't ever throw errors
    man <- newManager tlsManagerSettings
    let initState = IState {
              _queue = S.singleton req
            , _manager = man
            , _visited = S.empty
        }

    tst <- newTVarIO initState

    (output, input) <- spawn $ bounded 8
    as <- forM [1..10] $ \i ->
         async $ void $ (flip evalStateT) tst $ do runEffect $ crawlProducer >-> toOutput output
                                                   liftIO performGC


    a <- async $ void $ (flip evalStateT) tst $ runEffect $ fromInput input >-> getUrls >-> emailMatcher >-> validFilter >-> printer
    mapM_ wait (a:as)

main :: IO ()
main = do
    args <- parseArgs
    spider $ args ^. arg_startingUrl

