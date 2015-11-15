{-# LANGUAGE OverloadedStrings #-}
module StateMod where

import qualified Data.ByteString       as SB
import qualified Data.ByteString.Char8 as SBC

import qualified Data.Set              as S

import           Monad
import           State
import           Types

getNext :: Proxy a' a b' b Spider (Maybe Request)
getNext = do
    q <- gets queue
    if S.null q
    then return Nothing
    else do
        let (r, newq) = deleteFindMin q
        modify (\s -> s { queue = newq })
        return $ Just r

addRequest :: Request -> Pipe a b Spider ()
addRequest req = do
    q <- gets queue
    let newq = insert req q
    modify (\s -> s { queue = newq })

markVisited :: Request -> Proxy a' a b' b Spider ()
markVisited req = do
    s <- gets visited
    let uri = getUri req
    liftIO $ appendFile "/tmp/url.txt" $ (++ "\n") $ getUri req
    modify (\state -> state { visited = insert uri s })

shouldAdd :: Request -> Pipe a b Spider Bool
shouldAdd req = do
    b1 <- inQueue req
    b2 <- isVisited req
    return $ b1 || b2

inQueue :: Request -> Pipe a b Spider Bool
inQueue req = do
    let u = getUri req
    ls <- gets $ toList . queue
    return $ case find (\r -> getUri r == u) ls of
        Nothing -> True
        Just _  -> False

isVisited :: Request -> Pipe a b Spider Bool
isVisited req = do
    s <- gets visited
    let uri = getUri req
    return $ member uri s
