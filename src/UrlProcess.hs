module UrlProcess where

import           Monad
import           Types

import           Pipes (cat)

data ProcessorState = PState {
        _visited :: TVar (Set URI)
    }
type Processor = StateT ProcessorState HESS

makeLenses ''ProcessorState

urlProcess :: Pipe URI URI Processor ()
urlProcess = visitedFilter >-> tee visitedMarker

visitedFilter :: Pipe URI URI Processor ()
visitedFilter = forever $ do
    uri <- await
    visited <- isVisited uri
    if visited
    then return ()
    else yield uri


visitedMarker :: Consumer URI Processor ()
visitedMarker = forever $ do
    uri <- await
    f <- view visited_log
    liftIO $ appendFile f $ (++ "\n") $ show uri
    markVisited uri

isVisited :: URI -> Proxy a' a b' b Processor Bool
isVisited uri = do
    tvis <- use visited
    visSet <- liftIO $ atomically $ readTVar tvis
    return $ member uri visSet

markVisited :: URI -> Proxy a' a b' b Processor ()
markVisited uri = do
    tvis <- use visited
    liftIO $ atomically $ do
        visSet <- readTVar tvis
        let newSet = insert uri visSet
        writeTVar tvis newSet

