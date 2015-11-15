{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Fetch
import           MailScrape
import           State
import           Store
import           Types
import           UrlScrape

spider :: String -> IO ()
spider url = do
    req <- liftIO $ parseUrl url
    man <- newManager tlsManagerSettings
    let initState = State {
              queue = pushFront empty req
            , manager = man
        }
    evalStateT (runEffect $ crawlProducer >-> getUrls >-> emailMatcher >-> validFilter >-> printer) initState

main :: IO ()
main = spider "http://www.gnu.org/software/mailman/mailman-admin/node4.html"

