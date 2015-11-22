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

import qualified Data.Set           as S

import           System.Environment (getArgs)

spider :: String -> IO ()
spider url = do
    r <- liftIO $ parseUrl url
    let req = r { checkStatus = \s rh cj -> Nothing } -- Don't ever throw errors
    man <- newManager tlsManagerSettings
    let initState = State {
              queue = S.singleton req
            , manager = man
            , visited = S.empty
        }
    evalStateT (runEffect $ crawlProducer >-> getUrls >-> emailMatcher >-> validFilter >-> printer) initState

main :: IO ()
main = do
    args <- parseArgs
    spider $ arg_startingUrl args

