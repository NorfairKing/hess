{-# LANGUAGE OverloadedStrings #-}
module StateMod where

import qualified Data.Set as S

import           Monad
import           State
import           Types

getNext :: Proxy a' a b' b Spider (Maybe Request)
getNext = do
    q <- use queue
    if S.null q
    then return Nothing
    else do
        let (r, newq) = deleteFindMin q
        queue .= newq
        -- modify (\s -> s { queue = newq })
        return $ Just r

addRequest :: Request -> Pipe a b Spider ()
addRequest req = do
    queue %= insert req

markVisited :: Request -> Proxy a' a b' b Spider ()
markVisited req = do
    liftIO $ appendFile "/tmp/url.txt" $ (++ "\n") $ getUri req
    visited %= insert uri
  where uri = getUri req

shouldAdd :: Request -> Pipe a b Spider Bool
shouldAdd req = do
    b1 <- inQueue req
    b2 <- isVisited req
    return $ b1 || b2

inQueue :: Request -> Pipe a b Spider Bool
inQueue req = do
    let u = getUri req
    ls <- queue `uses` toList
    return $ case find (\r -> getUri r == u) ls of
        Nothing -> True
        Just _  -> False

isVisited :: Request -> Pipe a b Spider Bool
isVisited req = do
    visited `uses` member uri
  where uri = getUri req
