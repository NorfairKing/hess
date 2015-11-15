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

import qualified Data.Dequeue as D
import qualified Data.Set     as S

spider :: String -> IO ()
spider url = do
    r <- liftIO $ parseUrl url
    let req = r { checkStatus = \s rh cj -> Nothing } -- Don't ever throw errors
    man <- newManager tlsManagerSettings
    let initState = State {
              queue = pushFront D.empty req
            , manager = man
            , visited = S.empty
        }
    evalStateT (runEffect $ crawlProducer >-> getUrls >-> emailMatcher >-> validFilter >-> printer) initState

main :: IO ()
main = spider "http://www.gnu.org/software/mailman/mailman-admin/node4.html"

