module Fetch where

import           Data.ByteString.Lazy (ByteString)



import           Monad
import           State
import           StateMod
import           Types

crawlProducer :: Producer (Request, ByteString) Spider ()
crawlProducer = do
    mp <- getNext
    case mp of
        Nothing -> return ()
        Just req -> do
            markVisited req
            man <- gets manager


            resp <- liftIO $ httpLbs req man
            case statusCode $ responseStatus resp of
                200 -> do
                    let body = responseBody resp
                    yield (req, body)

                _   -> return ()

            crawlProducer
