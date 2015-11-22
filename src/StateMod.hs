{-# LANGUAGE OverloadedStrings #-}
module StateMod where

import qualified Data.Set as S

import           Monad
import           State
import           Types

modState :: (IState -> IState) -> Proxy a' a b' b Spider ()
modState f = do
    st <- get
    liftIO $ atomically $ modifyTVar' st f

modState' :: (IState -> (c, IState)) -> Proxy a' a b' b Spider c
modState' f = do
    st <- get
    liftIO $ atomically $ do
        s <- readTVar st
        let (r, ns) = f s
        writeTVar st ns
        return r


readStates :: (IState -> c) -> Proxy a' a b' b Spider c
readStates f = do
    st <- get
    liftIO $ atomically $ do
        s <- readTVar st
        return (f s)

getNext :: Proxy a' a b' b Spider (Maybe Request)
getNext = do
    empty <- readStates (S.null . _queue)
    if empty
    then return Nothing
    else do
        let ms :: IState -> (Request, IState)
            ms s = let q = _queue s
                       (r, nq) = deleteFindMin q
                   in (r, s & queue .~ nq)
        req <- modState' ms
        -- liftIO $ putStrLn $ "next: " ++ show req
        return $ Just req


addRequest :: Request -> Pipe a b Spider ()
addRequest req = do
    modState $ \q -> q & queue %~ insert req
    -- liftIO $ putStrLn $ "Adding: " ++ show req

markVisited :: Request -> Proxy a' a b' b Spider ()
markVisited req = do
    liftIO $ appendFile "/tmp/url.txt" $ (++ "\n") $ getUri req
    modState $ \q -> q & visited %~ insert uri
  where uri = getUri req

shouldAdd :: Request -> Pipe a b Spider Bool
shouldAdd req = do
    b1 <- inQueue req
    b2 <- isVisited req
    return $ b1 || b2

inQueue :: Request -> Pipe a b Spider Bool
inQueue req = do
    let u = getUri req
    ls <- readStates $ toList . _queue
    return $ case find (\r -> getUri r == u) ls of
        Nothing -> True
        Just _  -> False

isVisited :: Request -> Pipe a b Spider Bool
isVisited req = readStates $ member uri . _visited
  where uri = getUri req
