module UrlProcess where

import Monad
import Types

import Data.Maybe (fromJust)

import Pipes (cat)

{-# ANN module "HLint: ignore Use hierarchical imports" #-}

newtype ProcessorState = PState
    { onQueue :: TVar (Set URI)
    }

type Processor = StateT ProcessorState HESS

urlProcess :: Pipe URI URI Processor ()
urlProcess = do
    s <- asks stay_within_domain
    let domFilter =
            if s
                then domainFilter
                else cat
    domFilter >-> onQueueFilter >-> tee onQueueMarker

onQueueFilter :: Pipe URI URI Processor ()
onQueueFilter =
    forever $ do
        uri <- await
        onQueue_ <- isOnQueue uri
        unless onQueue_ $ yield uri

onQueueMarker :: Consumer URI Processor ()
onQueueMarker =
    forever $ do
        uri <- await
        l <- asks on_queue_logging
        when l $ do
            f <- asks on_queue_log_file
            liftIO $ appendFile f $ (++ "\n") $ show uri
        markOnQueue uri

isOnQueue :: URI -> Proxy a' a b' b Processor Bool
isOnQueue uri = do
    tvis <- gets onQueue
    visSet <- liftIO $ atomically $ readTVar tvis
    return $ member uri visSet

markOnQueue :: URI -> Proxy a' a b' b Processor ()
markOnQueue uri = do
    tvis <- gets onQueue
    liftIO $
        atomically $ do
            visSet <- readTVar tvis
            let newSet = insert uri visSet
            writeTVar tvis newSet

domainFilter :: Pipe URI URI Processor ()
domainFilter =
    forever $ do
        seed <- asks seed_uri
        uri <- await
        when (uriAuthority seed == uriAuthority uri && dom seed == dom uri) $
            yield uri
  where
    dom = uriRegName . fromJust . uriAuthority
