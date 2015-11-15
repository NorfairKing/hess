{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           TH

import           Control.Monad                (forM_, forever, when)
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.State          (StateT, evalStateT, get, modify,
                                               runStateT)
import qualified Data.ByteString              as SB
import           Data.ByteString.Lazy         (ByteString, isPrefixOf)
import qualified Data.ByteString.Lazy         as BS
import qualified Data.ByteString.Lazy.Char8   as BSLC
import           Network.HTTP.Client          (Manager (..), Request (..),
                                               closeManager, httpLbs,
                                               newManager, parseUrl,
                                               responseBody)
import           Network.HTTP.Client.Internal (setUriRelative)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.URI                  (parseURIReference)
import           Pipes
import           Pipes.Lift                   (evalStateP, runReaderP)
import           Pipes.Prelude                (tee)
import           Text.Email.Validate          (isValid)
import           Text.Regex.Posix             (getAllTextMatches, (=~))

import           Fetch
import           MailScrape
import           Monad
import           State
import           StateMod
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

