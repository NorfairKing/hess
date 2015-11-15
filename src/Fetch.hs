module Fetch where

import           Data.ByteString.Lazy (ByteString)



import           Monad
import           State
import           StateMod
import           Types

crawlProducer :: Producer (Request, ByteString) Spider ()
crawlProducer = do
    mp <- gets (popFront . queue)
    case mp of
        Nothing -> return ()
        Just (req, newQueue) -> do
            modify (\s -> s { queue = newQueue })
            markVisited req
            man <- gets manager


            resp <- liftIO $ httpLbs req man
            case statusCode $ responseStatus resp of
                200 -> do
                    let body = responseBody resp
                    yield (req, body)

                _   -> return ()

            crawlProducer
