{-# LANGUAGE OverloadedStrings #-}
module StateMod where

import qualified Data.ByteString       as SB
import qualified Data.ByteString.Char8 as SBC

import           Monad
import           State
import           Types

addRequest :: Request -> Pipe a b Spider ()
addRequest req = do
    q <- gets queue
    let newq = pushBack q req
    modify (\s -> s { queue = newq })

markVisited :: Request -> Proxy a' a b' b Spider ()
markVisited req = do
    s <- gets visited
    let uri = getUri req
    liftIO $ appendFile "/tmp/url.txt" $ (++ "\n") $ getUri req
    modify (\state -> state { visited = insert uri s })

isVisited :: Request -> Pipe a b Spider Bool
isVisited req = do
    s <- gets visited
    let uri = getUri req
    return $ member uri s

getUri :: Request -> String
getUri req = SBC.unpack $ SB.concat [prot, host req, path req]
    where prot = if secure req then "https://" else "http://"
