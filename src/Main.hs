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
    let initState = State {
              _queue = S.singleton req
            , _manager = man
            , _visited = S.empty
        }
    evalStateT (runEffect $ crawlProducer >-> getUrls >-> emailMatcher >-> validFilter >-> printer) initState

main :: IO ()
main = do
    args <- parseArgs
    spider $ args ^. arg_startingUrl

