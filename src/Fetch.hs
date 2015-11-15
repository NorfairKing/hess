module Fetch where

import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Client  (httpLbs, responseBody)

import           Monad
import           State
import           Types

crawlProducer :: Producer (Request, LB.ByteString) Spider ()
crawlProducer = do
    mp <- gets (popFront . queue)
    case mp of
        Nothing -> return ()
        Just (req, newQueue) -> do
            modify (\s -> s { queue = newQueue })
            man <- gets manager

            body <- liftIO $ responseBody `fmap` httpLbs req man
            yield (req, body)

            crawlProducer
