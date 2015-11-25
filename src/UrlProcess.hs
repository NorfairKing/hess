{-# LANGUAGE TemplateHaskell #-}
module UrlProcess where

import           Monad
import           Types

import           Data.Maybe (fromJust)

import           Pipes      (cat)

data ProcessorState = PState {
        _onQueue :: TVar (Set URI)
    }
type Processor = StateT ProcessorState HESS

makeLenses ''ProcessorState

urlProcess :: Pipe URI URI Processor ()
urlProcess = do
    s <- view stay_within_domain
    let domFilter = if s
                    then domainFilter
                    else cat
    domFilter >-> onQueueFilter >-> tee onQueueMarker

onQueueFilter :: Pipe URI URI Processor ()
onQueueFilter = forever $ do
    uri <- await
    onQueue <- isOnQueue uri
    if onQueue
    then return ()
    else yield uri


onQueueMarker :: Consumer URI Processor ()
onQueueMarker = forever $ do
    uri <- await
    f <- view on_queue_log
    liftIO $ appendFile f $ (++ "\n") $ show uri
    markOnQueue uri

isOnQueue :: URI -> Proxy a' a b' b Processor Bool
isOnQueue uri = do
    tvis <- use onQueue
    visSet <- liftIO $ atomically $ readTVar tvis
    return $ member uri visSet

markOnQueue :: URI -> Proxy a' a b' b Processor ()
markOnQueue uri = do
    tvis <- use onQueue
    liftIO $ atomically $ do
        visSet <- readTVar tvis
        let newSet = insert uri visSet
        writeTVar tvis newSet

domainFilter :: Pipe URI URI Processor ()
domainFilter = forever $ do
    seed <- view seed_uri
    uri <- await
    if uriAuthority seed == uriAuthority uri && dom seed == dom uri
    then yield uri
    else return ()
  where dom = uriRegName . fromJust . uriAuthority
